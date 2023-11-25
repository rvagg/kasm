pub mod ast;
pub mod parsable_bytes;
pub mod parsed_unit;
mod validate;

use std::io;
use std::u32;

pub fn parse(
    name: &str,
    bytes: &mut parsable_bytes::ParsableBytes,
) -> Result<parsed_unit::ParsedUnit, io::Error> {
    let mut unit = parsed_unit::ParsedUnit::new(name);

    read_header(bytes, &mut unit.magic, &mut unit.version)?;

    loop {
        if !bytes.has_at_least(1) {
            //TODO: assert we got the length we expected
            break;
        }

        let sec_id = bytes.read_byte()?;
        let sec_len = bytes.read_vu32()?;
        let mut sec_tag = "unknown".to_string();
        if sec_id == 0 {
            // custom section
            sec_tag = bytes.read_string()?;
        }

        if !bytes.has_at_least(sec_len as usize) {
            let error_message = format!(
                "not enough bytes left for section, expected {}, got {}",
                sec_len,
                bytes.remaining()
            );
            return Err(io::Error::new(io::ErrorKind::UnexpectedEof, error_message));
        }

        println!("section #{} '{}', len = {}", sec_id, sec_tag, sec_len);

        //TODO: check sec_len
        read_sections(sec_id, bytes, &mut unit)?;
    }

    Ok(unit)
}

fn read_sections(
    sec_id: u8,
    bytes: &mut parsable_bytes::ParsableBytes,
    unit: &mut parsed_unit::ParsedUnit,
) -> Result<(), io::Error> {
    match sec_id {
        1 => read_section_type(bytes, &mut unit.function_types),
        2 => read_section_import(bytes, &mut unit.imports),
        3 => read_section_function(bytes, &mut unit.functions, &unit.function_types),
        4 => read_section_table(bytes, &mut unit.table),
        5 => read_section_memory(bytes, &mut unit.memory),
        7 => read_section_export(bytes, &mut unit.exports),
        8 => read_section_start(bytes, &mut unit.start),
        10 => read_section_code(
            bytes,
            &mut unit.function_bodies,
            &unit.function_types,
            &unit.functions,
        ),
        11 => read_section_data(bytes, &mut unit.data),
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Unknown section type: #{}", sec_id),
            ))
        }
    }?;

    Ok(())
}

fn read_header(
    bytes: &mut parsable_bytes::ParsableBytes,
    magic: &mut u32,
    version: &mut u32,
) -> Result<(), io::Error> {
    *magic = bytes.read_u32()?;
    *version = bytes.read_u32()?;

    assert_eq!(*magic, 0x6d736100); // '∖0asm'
    println!("magic={}, version={}", magic, version);
    Ok(())
}

/* SECTION READERS ************************************************/

fn read_result_types(
    bytes: &mut parsable_bytes::ParsableBytes,
) -> Result<Vec<parsed_unit::ValueType>, io::Error> {
    let mut rt: Vec<parsed_unit::ValueType> = vec![];
    let count = bytes.read_vu64()?;
    for _ in 0..count {
        let value = parsed_unit::ValueType::decode(bytes.read_byte()?)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
        rt.push(value);
    }
    Ok(rt)
}

fn read_section_type(
    bytes: &mut parsable_bytes::ParsableBytes,
    function_types: &mut Vec<parsed_unit::FunctionType>,
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;

    for _ in 0..count {
        if bytes.read_byte()? != 0x60 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "expected 0x60 to lead function type",
            ));
        }
        let parameters = read_result_types(bytes)?;
        let return_types = read_result_types(bytes)?;

        function_types.push(parsed_unit::FunctionType {
            parameters: parameters,
            return_types: return_types,
        })
    }

    Ok(())
}

fn read_section_import(
    bytes: &mut parsable_bytes::ParsableBytes,
    imports: &mut Vec<parsed_unit::Import>,
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;

    for _ in 0..count {
        let module = bytes.read_string()?;
        let name = bytes.read_string()?;
        let external_kind = parsed_unit::ExternalKind::decode(bytes.read_byte()?)?;

        println!(
            "module = {}, name = {}, kind = {}",
            module, name, external_kind
        );

        let import = parsed_unit::Import {
            external_kind: external_kind,
            module: module,
            name: name,
            ftype_index: None,
            memory_type: None,
        };

        //if external_kind == parsed_unit::ExternalKind::Memory
        //  import.memory_type =

        imports.push(import);
    }

    Ok(())
}

fn read_section_function(
    bytes: &mut parsable_bytes::ParsableBytes,
    functions: &mut Vec<parsed_unit::Function>,
    function_types: &Vec<parsed_unit::FunctionType>,
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;

    for _ in 0..count {
        let ftype_index = bytes.read_byte()?;
        if ftype_index >= function_types.len() as u8 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "function type index out of range, expected < {}, got {}",
                    function_types.len(),
                    ftype_index
                ),
            ));
        }

        functions.push(parsed_unit::Function {
            ftype_index: ftype_index,
        })
    }

    Ok(())
}

fn read_section_memory(
    bytes: &mut parsable_bytes::ParsableBytes,
    memory: &mut Option<parsed_unit::Memory>,
) -> Result<(), io::Error> {
    let min = bytes.read_vu64()?;
    let max = bytes.read_vu64()?;
    let exported = bytes.read_bool()?; // going away in next version?

    *memory = Some(parsed_unit::Memory {
        min: min,
        max: max,
        exported: exported,
    });

    Ok(())
}

fn read_section_export(
    bytes: &mut parsable_bytes::ParsableBytes,
    exports: &mut Vec<parsed_unit::Export>,
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;

    for _ in 0..count {
        let name = bytes.read_string()?;
        let typ = bytes.read_byte()?;
        let idx = bytes.read_vu32()?;
        let index = parsed_unit::ExportIndex::decode(typ, idx)?;
        exports.push(parsed_unit::Export {
            index: index,
            name: name,
        })
    }

    Ok(())
}

fn read_section_code(
    bytes: &mut parsable_bytes::ParsableBytes,
    function_bodies: &mut Vec<parsed_unit::FunctionBody>,
    function_types: &Vec<parsed_unit::FunctionType>,
    functions: &Vec<parsed_unit::Function>,
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;
    if count != functions.len() as u64 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "function count mismatch, expected {}, got {}",
                functions.len(),
                count
            ),
        ));
    }

    for _ in 0..count {
        let mut locals: Vec<parsed_unit::ValueType> = vec![];
        let size = bytes.read_vu64()? as usize;
        let spos = bytes.pos();
        let locals_count = bytes.read_vu64()?;

        for _ in 0..locals_count {
            let local_n = bytes.read_vu64()?;
            let b = bytes.read_byte()?;
            for _ in 0..local_n {
                let value = parsed_unit::ValueType::decode(b)
                    .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
                locals.push(value);
            }
        }
        let lpos = bytes.pos();
        let body = bytes.read_bytes(size - (lpos - spos))?;

        let ftype = &function_types[functions[function_bodies.len()].ftype_index as usize];
        //TODO: parse function bodies
        let instructions: Vec<ast::Instruction> = self::ast::to_instructions(&ftype, &body)?;
        // println!("AST #{}: {}", i, ast);

        let function_body = parsed_unit::FunctionBody {
            locals: locals,
            body: body,
            instructions: instructions,
        };
        function_bodies.push(function_body);

        bytes.skip_to((spos as usize) + (size as usize));
    }

    Ok(())
}

fn read_section_data(
    bytes: &mut parsable_bytes::ParsableBytes,
    data: &mut Vec<parsed_unit::Data>,
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;

    for _ in 0..count {
        let address = bytes.read_vu64()?;
        let size = bytes.read_vu64()?;
        let bytes = bytes.read_bytes(size as usize)?;
        let d = parsed_unit::Data {
            address: address,
            data: bytes,
        };
        data.push(d);
    }

    Ok(())
}

fn read_section_table(
    bytes: &mut parsable_bytes::ParsableBytes,
    table: &mut Vec<u64>,
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;

    for _ in 0..count {
        let function_index = bytes.read_vu64()?;
        table.push(function_index)
    }

    Ok(())
}

fn read_section_start(
    bytes: &mut parsable_bytes::ParsableBytes,
    start: &mut u64,
) -> Result<(), io::Error> {
    let function_index = bytes.read_vu64()?;
    *start = function_index;
    Ok(())
}
