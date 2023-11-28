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

        let sec_name = match sec_id {
            0 => sec_tag.as_str(),
            1 => "type",
            2 => "import",
            3 => "function",
            4 => "table",
            5 => "memory",
            6 => "global",
            7 => "export",
            8 => "start",
            9 => "element",
            10 => "code",
            11 => "data",
            _ => "unknown",
        };
        println!("section #{} '{}', len = {}", sec_id, sec_name, sec_len);

        let start_pos = bytes.pos();
        read_sections(sec_id, bytes, &mut unit)?;
        let end_pos = bytes.pos();
        let actual_len = end_pos - start_pos;
        if actual_len != sec_len as usize {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "section length mismatch, expected {}, got {}",
                    sec_len, actual_len
                ),
            ));
        }
    }

    Ok(unit)
}

fn read_sections(
    sec_id: u8,
    bytes: &mut parsable_bytes::ParsableBytes,
    unit: &mut parsed_unit::ParsedUnit,
) -> Result<(), io::Error> {
    let start = bytes.pos();
    let mut positional: Option<&mut dyn parsed_unit::Positional> = None;

    match sec_id {
        1 => {
            read_section_type(bytes, &mut unit.types)?;
            positional = Some(&mut unit.types);
        }
        2 => {
            read_section_import(bytes, &mut unit.imports)?;
            positional = Some(&mut unit.imports);
        }
        3 => {
            read_section_function(bytes, &mut unit.functions, &unit.types)?;
            positional = Some(&mut unit.functions);
        }
        4 => {
            read_section_table(bytes, &mut unit.table)?;
            positional = Some(&mut unit.table);
        }
        5 => {
            read_section_memory(bytes, &mut unit.memory)?;
            positional = Some(&mut unit.memory);
        }
        6 => {
            read_section_global(bytes, &mut unit.globals)?;
            positional = Some(&mut unit.globals);
        }
        7 => {
            read_section_export(bytes, &mut unit.exports)?;
            positional = Some(&mut unit.exports);
        }
        8 => {
            read_section_start(bytes, &mut unit.start)?;
            positional = Some(&mut unit.start);
        }
        9 => {
            read_section_elements(bytes, &mut unit.elements)?;
            positional = Some(&mut unit.elements);
        }
        10 => {
            read_section_code(bytes, &mut unit.code, &unit.types, &unit.functions)?;
            positional = Some(&mut unit.code);
        }
        11 => {
            read_section_data(bytes, &mut unit.data)?;
            positional = Some(&mut unit.data);
        }
        12 => {
            read_section_datacount(bytes, &mut unit.data)?;
            positional = Some(&mut unit.data);
        }
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Unknown section type: #{}", sec_id),
            ))
        }
    };
    let end = bytes.pos();

    if let Some(pos) = positional {
        pos.set_position(start as u32, end as u32);
    }

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
    types: &mut parsed_unit::TypeSection,
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;

    for ii in 0..count {
        let byt = bytes.read_byte()?;
        if byt != 0x60 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "expected 0x60 to lead function type, got 0x{:02x} for func #{} / #{}",
                    byt, ii, count
                ),
            ));
        }
        let parameters = read_result_types(bytes)?;
        let return_types = read_result_types(bytes)?;

        types.push(parsed_unit::FunctionType {
            parameters: parameters,
            return_types: return_types,
        })
    }

    Ok(())
}

fn read_section_import(
    bytes: &mut parsable_bytes::ParsableBytes,
    imports: &mut parsed_unit::ImportSection,
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
    functions: &mut parsed_unit::FunctionSection,
    types: &parsed_unit::TypeSection,
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;

    for _ in 0..count {
        let ftype_index = bytes.read_byte()?;
        if ftype_index >= types.len() as u8 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "function type index out of range, expected < {}, got {}",
                    types.len(),
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
    memory: &mut parsed_unit::MemorySection,
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;

    for _ in 0..count {
        let limits = parsed_unit::Limits::decode(bytes)?;
        memory.push(parsed_unit::Memory {
            memory_type: limits,
        })
    }

    Ok(())
}

fn read_section_export(
    bytes: &mut parsable_bytes::ParsableBytes,
    exports: &mut parsed_unit::ExportSection,
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
    code: &mut parsed_unit::CodeSection,
    types: &parsed_unit::TypeSection,
    functions: &parsed_unit::FunctionSection,
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;
    if count != functions.functions.len() as u64 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "function count mismatch, expected {}, got {}",
                functions.functions.len(),
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
        let len = size - (bytes.pos() - spos);
        let body = bytes.read_bytes(size - (lpos - spos))?;
        let mut reader = parsable_bytes::ParsableBytes::new(body.clone());

        let ftype = match functions
            .get(code.len() as u8)
            .and_then(|f| types.get(f.ftype_index))
        {
            Some(ftype) => ftype,
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "error looking up function type for function #{}",
                    code.len()
                ),
            ))?,
        };

        let instructions: Vec<ast::Instruction> =
            self::ast::Instruction::decode_expression(&ftype, &mut reader)?;
        let function_body = parsed_unit::FunctionBody {
            locals: locals,
            // body: body,
            instructions: instructions,
            position: parsed_unit::SectionPosition {
                start: spos as u32,
                end: (spos + size) as u32,
            },
        };
        code.code.push(function_body);

        bytes.skip_to((spos as usize) + (size as usize));
    }

    Ok(())
}

impl parsed_unit::Data {
    pub fn decode(bytes: &mut parsable_bytes::ParsableBytes) -> Result<Self, io::Error> {
        let mut mode = parsed_unit::DataMode::Passive;
        // let mem_index: u32 = 0;
        // let expr : Vec<ast::Instruction> = vec![];

        let typ = bytes.read_vu32()?;
        match typ {
            0 => {
                mode = parsed_unit::DataMode::Active {
                    memory_index: 0,
                    offset: consume_constant_expr(bytes)?, // TODO: confirm this should be constant expr & signature type
                };
            }
            1 => {} // nothing else needed
            2 => {
                mode = parsed_unit::DataMode::Active {
                    memory_index: bytes.read_vu32()?,
                    offset: consume_constant_expr(bytes)?, // TODO: confirm this should be constant expr & signature type
                };
            }
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("unknown data type: 0x{:02x}", typ),
            ))?,
        };

        Ok(parsed_unit::Data {
            init: bytes.read_u8vec()?,
            mode: mode,
        })
    }
}

fn read_section_data(
    bytes: &mut parsable_bytes::ParsableBytes,
    datas: &mut parsed_unit::DataSection,
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;

    for _ in 0..count {
        let data = parsed_unit::Data::decode(bytes)?;
        println!("data type: {:?}", data);
        datas.data.push(data);
    }

    Ok(())
}

fn read_section_datacount(
    bytes: &mut parsable_bytes::ParsableBytes,
    datas: &mut parsed_unit::DataSection,
) -> Result<(), io::Error> {
    let count = bytes.read_vu32()?;
    if count != datas.data.len() as u32 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "data count mismatch, expected {}, got {}",
                datas.data.len(),
                count
            ),
        ));
    }
    Ok(())
}

impl parsed_unit::GlobalType {
    pub fn decode(bytes: &mut parsable_bytes::ParsableBytes) -> Result<Self, io::Error> {
        let value_type = parsed_unit::ValueType::decode(bytes.read_byte()?)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
        let mut_byte = bytes.read_byte()?;
        let mutable = match mut_byte {
            0x00 => false,
            0x01 => true,
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("unknown global type: 0x{:02x}", mut_byte),
            ))?,
        };
        Ok(parsed_unit::GlobalType {
            value_type: value_type,
            mutable: mutable,
        })
    }
}

impl parsed_unit::Global {
    pub fn decode(bytes: &mut parsable_bytes::ParsableBytes) -> Result<Self, io::Error> {
        Ok(parsed_unit::Global {
            global_type: parsed_unit::GlobalType::decode(bytes)?,
            init: consume_constant_expr(bytes)?, // TODO: confirm this should be constant expr & signature type
        })
    }
}

fn read_section_global(
    bytes: &mut parsable_bytes::ParsableBytes,
    globals: &mut parsed_unit::GlobalSection,
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;

    for _ in 0..count {
        let data = parsed_unit::Global::decode(bytes)?;
        println!("global: {:?}", data);
        globals.globals.push(data);
    }

    Ok(())
}

impl parsed_unit::RefType {
    pub fn decode(bytes: &mut parsable_bytes::ParsableBytes) -> Result<Self, io::Error> {
        let byte = bytes.read_byte()?;
        match byte {
            0x70 => Ok(parsed_unit::RefType::FuncRef),
            0x6f => Ok(parsed_unit::RefType::ExternRef),
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("unknown ref type: 0x{:02x}", byte),
            )),
        }
    }
}

impl parsed_unit::Limits {
    pub fn decode(bytes: &mut parsable_bytes::ParsableBytes) -> Result<Self, io::Error> {
        // if first byte is 0x00 then it's [0,0], if it's 0x01 then the next 2 u32's are [from,to]
        let typ = bytes.read_byte()?;
        let min = bytes.read_vu32()?;
        let mut max = u32::MAX;
        match typ {
            0x00 => {}
            0x01 => {
                max = bytes.read_vu32()?;
            }
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("unknown limits type: 0x{:02x}", typ),
            ))?,
        };
        Ok(parsed_unit::Limits { min, max })
    }
}

impl parsed_unit::TableType {
    pub fn decode(bytes: &mut parsable_bytes::ParsableBytes) -> Result<Self, io::Error> {
        // reftype then limits
        let ref_type = parsed_unit::RefType::decode(bytes)?;
        let limits = parsed_unit::Limits::decode(bytes)?;
        Ok(parsed_unit::TableType {
            ref_type: ref_type,
            limits: limits,
        })
    }
}

fn read_section_table(
    bytes: &mut parsable_bytes::ParsableBytes,
    table: &mut parsed_unit::TableSection,
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;

    for _ in 0..count {
        let table_type = parsed_unit::TableType::decode(bytes)?;
        println!("table type: {:?}", table_type);
        table.table.push(table_type);
    }

    Ok(())
}

fn read_section_start(
    bytes: &mut parsable_bytes::ParsableBytes,
    start: &mut parsed_unit::StartSection,
) -> Result<(), io::Error> {
    start.start = bytes.read_vu64()?;
    Ok(())
}

impl parsed_unit::Element {
    pub fn decode(bytes: &mut parsable_bytes::ParsableBytes) -> Result<Self, io::Error> {
        let mut read_type =
            |bytes: &mut parsable_bytes::ParsableBytes| -> Result<parsed_unit::RefType, io::Error> {
                let byte = bytes.read_byte()?;
                match byte {
                    0x70 => Ok(parsed_unit::RefType::FuncRef),
                    0x6f => Ok(parsed_unit::RefType::ExternRef),
                    _ => Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!("unknown element ref type: 0x{:02x}", byte),
                    )),
                }
            };
        let mut read_element_kind =
            |bytes: &mut parsable_bytes::ParsableBytes| -> Result<parsed_unit::RefType, io::Error> {
                let byte = bytes.read_byte()?;
                match byte {
                    0x00 => Ok(parsed_unit::RefType::FuncRef),
                    _ => Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!("unknown element element kind: 0x{:02x}", byte),
                    )),
                }
            };
        let mut read_table_index =
            |bytes: &mut parsable_bytes::ParsableBytes| -> Result<u32, io::Error> {
                bytes.read_vu32()
            };
        let mut read_init = |bytes: &mut parsable_bytes::ParsableBytes| -> Result<Vec<Vec<ast::Instruction>>, io::Error> {
            let count = bytes.read_vu32()?;
            let mut init: Vec<Vec<ast::Instruction>> = vec![];
            for _ in 0..count {
                // TODO: does this need to be a constant expression? probably, also confirm signautre
                init.push(consume_constant_expr(bytes)?);
            }
            Ok(init)
        };
        let mut read_func_indexes =
            |bytes: &mut parsable_bytes::ParsableBytes| -> Result<Vec<u32>, io::Error> {
                let count = bytes.read_vu32()?;
                let mut func_indexes: Vec<u32> = vec![];
                for _ in 0..count {
                    let func_index = bytes.read_vu32()?;
                    func_indexes.push(func_index);
                }
                Ok(func_indexes)
            };
        let mut read_func_index_init = |bytes: &mut parsable_bytes::ParsableBytes| -> Result<Vec<Vec<ast::Instruction>>, io::Error> {
            let fi = read_func_indexes(bytes)?;
            let mut init: Vec<Vec<ast::Instruction>> = vec![];
            Ok(fi
                .into_iter()
                .map(|func_index| {
                    vec![
                        ast::Instruction::new(
                            ast::InstructionType::RefFunc,
                            ast::InstructionData::FunctionInstruction {
                                function_index: func_index,
                            },
                        ),
                        ast::Instruction::new(
                            ast::InstructionType::End,
                            ast::InstructionData::SimpleInstruction,
                        ),
                    ]
                })
                .collect())
        };

        let typ = bytes.read_vu32()?;
        match typ {
            0 => {
                // 0:u32 𝑒:expr 𝑦*:vec(funcidx) => {type funcref, init ((ref.func 𝑦) end)*, mode active {table 0, offset 𝑒}}
                Ok(parsed_unit::Element {
                    ref_type: parsed_unit::RefType::FuncRef,
                    mode: parsed_unit::ElementMode::Active {
                        table_index: 0,
                        offset: consume_constant_expr(bytes)?,
                    },
                    init: read_func_index_init(bytes)?,
                })
            }
            1 => {
                // 1:u32 et : elemkind 𝑦*:vec(funcidx) => {type et, init ((ref.func 𝑦) end)*, mode passive}
                Ok(parsed_unit::Element {
                    ref_type: read_element_kind(bytes)?,
                    mode: parsed_unit::ElementMode::Passive,
                    init: read_func_index_init(bytes)?,
                })
            }
            2 => {
                // 2:u32 𝑥:tableidx 𝑒:expr et : elemkind 𝑦*:vec(funcidx) => {type et, init ((ref.func 𝑦) end)*, mode active {table 𝑥, offset 𝑒}}
                Ok(parsed_unit::Element {
                    mode: parsed_unit::ElementMode::Active {
                        table_index: read_table_index(bytes)?,
                        offset: consume_constant_expr(bytes)?, // TODO: confirm is constant expr && signautre type
                    },
                    ref_type: read_element_kind(bytes)?,
                    init: read_func_index_init(bytes)?,
                })
            }
            3 => {
                // 3:u32 et : elemkind 𝑦*:vec(funcidx) => {type et, init ((ref.func 𝑦) end)*, mode declarative}
                Ok(parsed_unit::Element {
                    ref_type: read_element_kind(bytes)?,
                    mode: parsed_unit::ElementMode::Declarative,
                    init: read_func_index_init(bytes)?,
                })
            }
            4 => {
                // 4:u32 𝑒:expr el *:vec(expr) => {type funcref, init el *, mode active {table 0, offset 𝑒}}
                Ok(parsed_unit::Element {
                    ref_type: parsed_unit::RefType::FuncRef,
                    mode: parsed_unit::ElementMode::Active {
                        table_index: 0,
                        offset: consume_constant_expr(bytes)?, // TODO: confirm is constant expr && signautre type
                    },
                    init: read_init(bytes)?,
                })
            }
            5 => {
                // 5:u32 et : reftype el *:vec(expr) => {type 𝑒𝑡, init el *, mode passive}
                Ok(parsed_unit::Element {
                    ref_type: read_type(bytes)?,
                    mode: parsed_unit::ElementMode::Passive,
                    init: read_init(bytes)?,
                })
            }
            6 => {
                // 6:u32 𝑥:tableidx 𝑒:expr et : reftype el *:vec(expr) => {type 𝑒𝑡, init el *, mode active {table 𝑥, offset 𝑒}}
                Ok(parsed_unit::Element {
                    mode: parsed_unit::ElementMode::Active {
                        table_index: read_table_index(bytes)?,
                        offset: consume_constant_expr(bytes)?, // TODO: confirm is constant expr && signautre type
                    },
                    ref_type: read_type(bytes)?,
                    init: read_init(bytes)?,
                })
            }
            7 => {
                // 7:u32 et : reftype el *:vec(expr) => {type 𝑒𝑡, init el *, mode declarative}
                Ok(parsed_unit::Element {
                    ref_type: read_type(bytes)?,
                    mode: parsed_unit::ElementMode::Declarative,
                    init: read_init(bytes)?,
                })
            }
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("unknown element type: 0x{:02x}", typ),
            )),
        }
    }
}

fn read_section_elements(
    bytes: &mut parsable_bytes::ParsableBytes,
    elements: &mut parsed_unit::ElementSection,
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;

    for _ in 0..count {
        let element = parsed_unit::Element::decode(bytes)?;
        println!("table type: {:?}", element);
        elements.push(element);
    }

    Ok(())
}

fn consume_constant_expr(
    bytes: &mut parsable_bytes::ParsableBytes,
) -> Result<Vec<ast::Instruction>, io::Error> {
    self::ast::Instruction::decode_expression(
        &parsed_unit::FunctionType {
            // TODO: confirm this is the right signature for these: [0,n]=>[x]
            parameters: vec![parsed_unit::ValueType::I32, parsed_unit::ValueType::I32],
            return_types: vec![parsed_unit::ValueType::I32],
        },
        bytes,
    )
}
