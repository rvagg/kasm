mod ast;
pub mod parsable_bytes;
pub mod parsed_unit;

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

        let sec_id = bytes.read_vu64()?;
        let sec_len = bytes.read_vu64()? as usize;
        let mut sec_tag = "unknown".to_string();
        if sec_id == 0 {
            sec_tag = bytes.read_string()?;
        }

        if !bytes.has_at_least(sec_len) {
            let error_message = format!(
                "not enough bytes left for section, expected {}, got {}",
                sec_len,
                bytes.remaining()
            );
            return Err(io::Error::new(io::ErrorKind::UnexpectedEof, error_message));
        }

        println!(
            "section #{} '{}', len = {}",
            sec_id,
            sec_tag,
            sec_len
        );

        //TODO: check sec_len
        read_sections(sec_id, sec_tag, bytes, &mut unit)?;
    }

    Ok(unit)
}

fn read_sections(
    sec_id: u64,
    sec_tag: String,
    bytes: &mut parsable_bytes::ParsableBytes,
    unit: &mut parsed_unit::ParsedUnit,
) -> Result<(), io::Error> {
    match sec_id {
        1 => read_section_type(bytes, &mut unit.types),
        2 => read_section_import(bytes, &mut unit.imports),
        3 => read_section_function(bytes, &mut unit.functions),
        4 => read_section_table(bytes, &mut unit.table),
        5 => read_section_memory(bytes, &mut unit.memory),
        7 => read_section_export(bytes, &mut unit.exports),
        8 => read_section_start(bytes, &mut unit.start),
        10 => read_section_code(bytes, &mut unit.function_bodies),
        11 => read_section_data(bytes, &mut unit.data),
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Unknown section type: #{} '{}'", sec_id, sec_tag),
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

    assert_eq!(*magic, 0x6d736100);
    println!("magic={}, version={}", magic, version);
    Ok(())
}

/* SECTION READERS ************************************************/

fn read_section_type(
    bytes: &mut parsable_bytes::ParsableBytes,
    types: &mut Vec<parsed_unit::Type>,
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;

    for _ in 0..count {
        let func_form = bytes.read_vs64()?;
        assert_eq!(func_form, -0x20);
        let mut parameters: Vec<parsed_unit::ValueType> = vec![];
        let parameter_count = bytes.read_vu64()?;
        for _ in 0..parameter_count {
            parameters.push(parsed_unit::ValueType::decode(bytes.read_byte()?)?);
        }

        let return_type: parsed_unit::ValueType = match bytes.read_byte()? {
            0 => parsed_unit::ValueType::None,
            b @ _ => {
                assert_eq!(b, 1);
                parsed_unit::ValueType::decode(bytes.read_byte()?)? // TODO: b != 1 = fail
            }
        };

        types.push(parsed_unit::Type {
            form: parsed_unit::Form::Function,
            parameters: parameters,
            return_type: return_type,
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
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;

    for _ in 0..count {
        let ftype_index = bytes.read_byte()?;

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
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;

    for i in 0..count {
        let mut locals: Vec<parsed_unit::ValueType> = vec![];
        let size = bytes.read_vu64()? as usize;
        let spos = bytes.pos();
        let locals_count = bytes.read_vu64()?;

        for _ in 0..locals_count {
            let local_n = bytes.read_vu64()?;
            let b = bytes.read_byte()?;
            for _ in 0..local_n {
                locals.push(parsed_unit::ValueType::decode(b)?);
            }
        }
        let lpos = bytes.pos();
        let body = bytes.read_bytes(size - (lpos - spos))?;

        //TODO: parse function bodies
        let ast = self::ast::to_ast(&body)?;
        println!("AST #{}: {}", i, ast);

        let function_body = parsed_unit::FunctionBody {
            locals: locals,
            body: body,
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
