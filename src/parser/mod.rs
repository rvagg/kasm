pub mod parsable_bytes;
pub mod parsed_unit;
mod ast;

use std::u32;

/*pub fn parse (name : &String, bytes : parsable_bytes::ParsableBytes) -> parsed_unit::ParsedUnit {
  let mut parser = parser::Parser::new(name, bytes);
  parser.parse();
  parser.unit
}*/

pub fn parse(name : &String, bytes : &mut parsable_bytes::ParsableBytes) -> parsed_unit::ParsedUnit {
  let mut unit = parsed_unit::ParsedUnit::new(name);

  read_header(bytes, &mut unit.magic, &mut unit.version);

  loop {
    if !bytes.has_at_least(1) {
      //TODO: assert we got the length we expected
      break;
    }

    let sec_id = bytes.read_vu64();
    let sec_len = bytes.read_vu64() as usize;
    let mut sec_tag = "unkown".to_string();
    if sec_id == 0 {
      sec_tag = bytes.read_string();
    }

    if !bytes.has_at_least(sec_len) {
      //TODO: error
      println!("not enough bytes left for section");
      break;
    }

    //TODO: check sec_len
    match sec_id {
        1 => read_section_type(bytes, &mut unit.types)
      , 2 => read_section_import(bytes, &mut unit.imports)
      , 3 => read_section_function(bytes, &mut unit.functions)
      , 4 => read_section_table(bytes, &mut unit.table)
      , 5 => read_section_memory(bytes, &mut unit.memory)
      , 8 => read_section_start(bytes, &mut unit.start)
      , 10 => read_section_code(bytes, &mut unit.function_bodies)
      , 11 => read_section_data(bytes, &mut unit.data)
      , _ => {
          println!("Unknown section type: #{} '{}'", sec_id, sec_tag);
          break;
        }
    }
  }

  unit
}


fn read_header(bytes : &mut parsable_bytes::ParsableBytes, magic : &mut u32, version : &mut u32) {
  *magic   = bytes.read_u32();
  *version = bytes.read_u32();

  assert_eq!(*magic, 0x6d736100);
  println!("magic={}, version={}", magic, version);
}


/* SECTION READERS ************************************************/

fn read_section_type(bytes : &mut parsable_bytes::ParsableBytes, types : &mut Vec<parsed_unit::Type>) {
  let count = bytes.read_vu64();

  for _ in 0..count {
    let func_form = bytes.read_vs64();
    assert_eq!(func_form, -0x20);
    let mut parameters : Vec<parsed_unit::ValueType> = vec!();
    let parameter_count = bytes.read_vu64();
    for _ in 0..parameter_count {
      parameters.push(parsed_unit::ValueType::decode(bytes.read_byte()));
    }

    let return_type : parsed_unit::ValueType = match bytes.read_byte() {
        0 => parsed_unit::ValueType::None
      , b @ _ => {
          assert_eq!(b, 1);
          parsed_unit::ValueType::decode(bytes.read_byte()) // TODO: b != 1 = fail
        }
    };

    types.push(parsed_unit::Type {
        form        : parsed_unit::Form::Function
      , parameters  : parameters
      , return_type : return_type
    })
  }
}


fn read_section_import(bytes : &mut parsable_bytes::ParsableBytes, imports : &mut Vec<parsed_unit::Import>) {
  let count = bytes.read_vu64();

  for _ in 0..count {
    let module = bytes.read_string();
    let name = bytes.read_string();
    let external_kind = parsed_unit::ExternalKind::decode(bytes.read_byte());

    println!("module = {}, name = {}, kind = {}", module, name, external_kind);

    let import = parsed_unit::Import {
        external_kind : external_kind
      , module        : module
      , name          : name
      , ftype_index   : None
      , memory_type   : None
    };

    //if external_kind == parsed_unit::ExternalKind::Memory
    //  import.memory_type = 

    imports.push(import);
  }
}


fn read_section_function(bytes : &mut parsable_bytes::ParsableBytes, functions : &mut Vec<parsed_unit::Function>) {
  let count = bytes.read_vu64();

  for _ in 0..count {
    let ftype_index = bytes.read_byte();

    functions.push(parsed_unit::Function {
        ftype_index : ftype_index
    })
  }
}


fn read_section_memory(bytes : &mut parsable_bytes::ParsableBytes, memory : &mut Option<parsed_unit::Memory>) {
  let min = bytes.read_vu64();
  let max = bytes.read_vu64();
  let exported = bytes.read_bool(); // going away in next version?

  *memory = Some(parsed_unit::Memory {
      min      : min
    , max      : max
    , exported : exported
  })
}


fn read_section_export(bytes : &mut parsable_bytes::ParsableBytes, exports : &mut Vec<parsed_unit::Export>) {
  let count = bytes.read_vu64();

  for _ in 0..count {
    let function_index = bytes.read_vu64();
    let name = bytes.read_string();

    exports.push(parsed_unit::Export {
        function_index : function_index
      , name           : name
    })
  }
}


fn read_section_code(bytes : &mut parsable_bytes::ParsableBytes, function_bodies : &mut Vec<parsed_unit::FunctionBody>) {
  let count = bytes.read_vu64();

  for i in 0..count {
    let mut locals : Vec<parsed_unit::ValueType> = vec!();
    let size = bytes.read_vu64() as usize;
    let spos = bytes.pos();
    let locals_count = bytes.read_vu64();

    for _ in 0..locals_count {
      let local_n = bytes.read_vu64();
      let b = bytes.read_byte();
      for _ in 0..local_n {
        locals.push(parsed_unit::ValueType::decode(b));
      }
    }
    let lpos = bytes.pos();
    let body = bytes.read_bytes(size - (lpos - spos));

    //TODO: parse function bodies
    let ast = self::ast::to_ast(&body);
    println!("AST #{}: {}", i, ast);

    let function_body = parsed_unit::FunctionBody {
        locals : locals
      , body   : body
    };
    function_bodies.push(function_body);

    bytes.skip_to((spos as usize) + (size as usize));
  }
}


fn read_section_data(bytes : &mut parsable_bytes::ParsableBytes, data : &mut Vec<parsed_unit::Data>) {
  let count = bytes.read_vu64();

  for _ in 0..count {
    let address = bytes.read_vu64();
    let size = bytes.read_vu64();
    let bytes = bytes.read_bytes(size as usize);
    let d = parsed_unit::Data {
        address : address
      , data    : bytes
    };
    data.push(d);
  }
}


fn read_section_table(bytes : &mut parsable_bytes::ParsableBytes, table : &mut Vec<u64>) {
  let count = bytes.read_vu64();

  for _ in 0..count {
    let function_index = bytes.read_vu64();
    table.push(function_index)
  }
}


fn read_section_start(bytes : &mut parsable_bytes::ParsableBytes, start : &mut u64) {
  let function_index = bytes.read_vu64();
  *start = function_index
}
