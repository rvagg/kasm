pub mod ast;
pub mod module;
pub mod reader;
mod validate;

use std::io;
use std::u32;

pub fn parse(name: &str, bytes: &mut reader::Reader) -> Result<module::Module, io::Error> {
    let mut unit = module::Module::new(name);

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
    bytes: &mut reader::Reader,
    unit: &mut module::Module,
) -> Result<(), io::Error> {
    let start = bytes.pos();
    let positional: &mut dyn module::Positional = match sec_id {
        1 => {
            read_section_type(bytes, &mut unit.types)?;
            &mut unit.types as &mut dyn module::Positional
        }
        2 => {
            read_section_import(bytes, &mut unit.imports)?;
            &mut unit.imports as &mut dyn module::Positional
        }
        3 => {
            read_section_function(bytes, &mut unit.functions, &unit.types)?;
            &mut unit.functions as &mut dyn module::Positional
        }
        4 => {
            read_section_table(bytes, &mut unit.table)?;
            &mut unit.table as &mut dyn module::Positional
        }
        5 => {
            read_section_memory(bytes, &mut unit.memory)?;
            &mut unit.memory as &mut dyn module::Positional
        }
        6 => {
            read_section_global(bytes, &mut unit.globals)?;
            &mut unit.globals as &mut dyn module::Positional
        }
        7 => {
            read_section_export(bytes, &mut unit.exports)?;
            &mut unit.exports as &mut dyn module::Positional
        }
        8 => {
            read_section_start(bytes, &mut unit.start)?;
            &mut unit.start as &mut dyn module::Positional
        }
        9 => {
            read_section_elements(bytes, &mut unit.elements)?;
            &mut unit.elements as &mut dyn module::Positional
        }
        10 => {
            read_section_code(bytes, &mut unit.code, &unit.types, &unit.functions)?;
            &mut unit.code as &mut dyn module::Positional
        }
        11 => {
            read_section_data(bytes, &mut unit.data)?;
            &mut unit.data as &mut dyn module::Positional
        }
        12 => {
            read_section_datacount(bytes, &mut unit.data)?;
            &mut unit.data as &mut dyn module::Positional
        }
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("Unknown section type: #{}", sec_id),
            ))
        }
    };
    let end = bytes.pos();
    positional.set_position(start as u32, end as u32);

    Ok(())
}

fn read_header(
    bytes: &mut reader::Reader,
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

fn read_result_types(bytes: &mut reader::Reader) -> Result<Vec<module::ValueType>, io::Error> {
    let mut rt: Vec<module::ValueType> = vec![];
    let count = bytes.read_vu64()?;
    for _ in 0..count {
        let value = module::ValueType::decode(bytes.read_byte()?)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
        rt.push(value);
    }
    Ok(rt)
}

fn read_section_type(
    bytes: &mut reader::Reader,
    types: &mut module::TypeSection,
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

        types.push(module::FunctionType {
            parameters: parameters,
            return_types: return_types,
        })
    }

    Ok(())
}

fn read_section_import(
    bytes: &mut reader::Reader,
    imports: &mut module::ImportSection,
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;

    for _ in 0..count {
        let module = bytes.read_string()?;
        let name = bytes.read_string()?;
        let external_kind = module::ExternalKind::decode(bytes.read_byte()?)?;

        println!(
            "module = {}, name = {}, kind = {}",
            module, name, external_kind
        );

        let import = module::Import {
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
    bytes: &mut reader::Reader,
    functions: &mut module::FunctionSection,
    types: &module::TypeSection,
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

        functions.push(module::Function {
            ftype_index: ftype_index,
        })
    }

    Ok(())
}

fn read_section_memory(
    bytes: &mut reader::Reader,
    memory: &mut module::MemorySection,
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;

    for _ in 0..count {
        let limits = module::Limits::decode(bytes)?;
        memory.push(module::Memory {
            memory_type: limits,
        })
    }

    Ok(())
}

fn read_section_export(
    bytes: &mut reader::Reader,
    exports: &mut module::ExportSection,
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;

    for _ in 0..count {
        let name = bytes.read_string()?;
        let typ = bytes.read_byte()?;
        let idx = bytes.read_vu32()?;
        let index = module::ExportIndex::decode(typ, idx)?;
        exports.push(module::Export {
            index: index,
            name: name,
        })
    }

    Ok(())
}

fn read_section_code(
    bytes: &mut reader::Reader,
    code: &mut module::CodeSection,
    types: &module::TypeSection,
    functions: &module::FunctionSection,
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
        let mut locals: Vec<module::ValueType> = vec![];
        let size = bytes.read_vu64()? as usize;
        let spos = bytes.pos();
        let locals_count = bytes.read_vu64()?;

        for _ in 0..locals_count {
            let local_n = bytes.read_vu64()?;
            let b = bytes.read_byte()?;
            for _ in 0..local_n {
                let value = module::ValueType::decode(b)
                    .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
                locals.push(value);
            }
        }
        let lpos = bytes.pos();
        let body = bytes.read_bytes(size - (lpos - spos))?;
        let mut reader = reader::Reader::new(body.clone());

        let instructions: Vec<ast::Instruction> = self::ast::Instruction::decode_function(
            &types,
            &functions,
            &locals,
            code.len() as u32,
            &mut reader,
        )?;
        let function_body = module::FunctionBody {
            locals: locals,
            // body: body,
            instructions: instructions,
            position: module::SectionPosition {
                start: spos as u32,
                end: (spos + size) as u32,
            },
        };
        code.code.push(function_body);

        bytes.skip_to((spos as usize) + (size as usize));
    }

    Ok(())
}

impl module::Data {
    pub fn decode(bytes: &mut reader::Reader) -> Result<Self, io::Error> {
        let mut mode = module::DataMode::Passive;
        // let mem_index: u32 = 0;
        // let expr : Vec<ast::Instruction> = vec![];

        let typ = bytes.read_vu32()?;
        match typ {
            0 => {
                mode = module::DataMode::Active {
                    memory_index: 0,
                    offset: consume_constant_expr(bytes)?, // TODO: confirm this should be constant expr & signature type
                };
            }
            1 => {} // nothing else needed
            2 => {
                mode = module::DataMode::Active {
                    memory_index: bytes.read_vu32()?,
                    offset: consume_constant_expr(bytes)?, // TODO: confirm this should be constant expr & signature type
                };
            }
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("unknown data type: 0x{:02x}", typ),
            ))?,
        };

        Ok(module::Data {
            init: bytes.read_u8vec()?,
            mode: mode,
        })
    }
}

fn read_section_data(
    bytes: &mut reader::Reader,
    datas: &mut module::DataSection,
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;

    for _ in 0..count {
        let data = module::Data::decode(bytes)?;
        println!("data type: {:?}", data);
        datas.data.push(data);
    }

    Ok(())
}

fn read_section_datacount(
    bytes: &mut reader::Reader,
    datas: &mut module::DataSection,
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

impl module::GlobalType {
    pub fn decode(bytes: &mut reader::Reader) -> Result<Self, io::Error> {
        let value_type = module::ValueType::decode(bytes.read_byte()?)
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
        Ok(module::GlobalType {
            value_type: value_type,
            mutable: mutable,
        })
    }
}

impl module::Global {
    pub fn decode(bytes: &mut reader::Reader) -> Result<Self, io::Error> {
        Ok(module::Global {
            global_type: module::GlobalType::decode(bytes)?,
            init: consume_constant_expr(bytes)?, // TODO: confirm this should be constant expr & signature type
        })
    }
}

fn read_section_global(
    bytes: &mut reader::Reader,
    globals: &mut module::GlobalSection,
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;

    for _ in 0..count {
        let data = module::Global::decode(bytes)?;
        println!("global: {:?}", data);
        globals.globals.push(data);
    }

    Ok(())
}

impl module::RefType {
    pub fn decode(bytes: &mut reader::Reader) -> Result<Self, io::Error> {
        let byte = bytes.read_byte()?;
        match byte {
            0x70 => Ok(module::RefType::FuncRef),
            0x6f => Ok(module::RefType::ExternRef),
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("unknown ref type: 0x{:02x}", byte),
            )),
        }
    }
}

impl module::Limits {
    pub fn decode(bytes: &mut reader::Reader) -> Result<Self, io::Error> {
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
        Ok(module::Limits { min, max })
    }
}

impl module::TableType {
    pub fn decode(bytes: &mut reader::Reader) -> Result<Self, io::Error> {
        // reftype then limits
        let ref_type = module::RefType::decode(bytes)?;
        let limits = module::Limits::decode(bytes)?;
        Ok(module::TableType {
            ref_type: ref_type,
            limits: limits,
        })
    }
}

fn read_section_table(
    bytes: &mut reader::Reader,
    table: &mut module::TableSection,
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;

    for _ in 0..count {
        let table_type = module::TableType::decode(bytes)?;
        println!("table type: {:?}", table_type);
        table.table.push(table_type);
    }

    Ok(())
}

fn read_section_start(
    bytes: &mut reader::Reader,
    start: &mut module::StartSection,
) -> Result<(), io::Error> {
    start.start = bytes.read_vu64()?;
    Ok(())
}

impl module::Element {
    pub fn decode(bytes: &mut reader::Reader) -> Result<Self, io::Error> {
        let read_type = |bytes: &mut reader::Reader| -> Result<module::RefType, io::Error> {
            let byte = bytes.read_byte()?;
            match byte {
                0x70 => Ok(module::RefType::FuncRef),
                0x6f => Ok(module::RefType::ExternRef),
                _ => Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("unknown element ref type: 0x{:02x}", byte),
                )),
            }
        };
        let read_element_kind = |bytes: &mut reader::Reader| -> Result<module::RefType, io::Error> {
            let byte = bytes.read_byte()?;
            match byte {
                0x00 => Ok(module::RefType::FuncRef),
                _ => Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("unknown element element kind: 0x{:02x}", byte),
                )),
            }
        };
        let read_table_index =
            |bytes: &mut reader::Reader| -> Result<u32, io::Error> { bytes.read_vu32() };
        let read_init =
            |bytes: &mut reader::Reader| -> Result<Vec<Vec<ast::Instruction>>, io::Error> {
                let count = bytes.read_vu32()?;
                let mut init: Vec<Vec<ast::Instruction>> = vec![];
                for _ in 0..count {
                    // TODO: does this need to be a constant expression? probably, also confirm signautre
                    init.push(consume_constant_expr(bytes)?);
                }
                Ok(init)
            };
        let read_func_indexes = |bytes: &mut reader::Reader| -> Result<Vec<u32>, io::Error> {
            let count = bytes.read_vu32()?;
            let mut func_indexes: Vec<u32> = vec![];
            for _ in 0..count {
                let func_index = bytes.read_vu32()?;
                func_indexes.push(func_index);
            }
            Ok(func_indexes)
        };
        let read_func_index_init =
            |bytes: &mut reader::Reader| -> Result<Vec<Vec<ast::Instruction>>, io::Error> {
                let fi = read_func_indexes(bytes)?;
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
                Ok(module::Element {
                    ref_type: module::RefType::FuncRef,
                    mode: module::ElementMode::Active {
                        table_index: 0,
                        offset: consume_constant_expr(bytes)?,
                    },
                    init: read_func_index_init(bytes)?,
                })
            }
            1 => {
                // 1:u32 et : elemkind 𝑦*:vec(funcidx) => {type et, init ((ref.func 𝑦) end)*, mode passive}
                Ok(module::Element {
                    ref_type: read_element_kind(bytes)?,
                    mode: module::ElementMode::Passive,
                    init: read_func_index_init(bytes)?,
                })
            }
            2 => {
                // 2:u32 𝑥:tableidx 𝑒:expr et : elemkind 𝑦*:vec(funcidx) => {type et, init ((ref.func 𝑦) end)*, mode active {table 𝑥, offset 𝑒}}
                Ok(module::Element {
                    mode: module::ElementMode::Active {
                        table_index: read_table_index(bytes)?,
                        offset: consume_constant_expr(bytes)?, // TODO: confirm is constant expr && signautre type
                    },
                    ref_type: read_element_kind(bytes)?,
                    init: read_func_index_init(bytes)?,
                })
            }
            3 => {
                // 3:u32 et : elemkind 𝑦*:vec(funcidx) => {type et, init ((ref.func 𝑦) end)*, mode declarative}
                Ok(module::Element {
                    ref_type: read_element_kind(bytes)?,
                    mode: module::ElementMode::Declarative,
                    init: read_func_index_init(bytes)?,
                })
            }
            4 => {
                // 4:u32 𝑒:expr el *:vec(expr) => {type funcref, init el *, mode active {table 0, offset 𝑒}}
                Ok(module::Element {
                    ref_type: module::RefType::FuncRef,
                    mode: module::ElementMode::Active {
                        table_index: 0,
                        offset: consume_constant_expr(bytes)?, // TODO: confirm is constant expr && signautre type
                    },
                    init: read_init(bytes)?,
                })
            }
            5 => {
                // 5:u32 et : reftype el *:vec(expr) => {type 𝑒𝑡, init el *, mode passive}
                Ok(module::Element {
                    ref_type: read_type(bytes)?,
                    mode: module::ElementMode::Passive,
                    init: read_init(bytes)?,
                })
            }
            6 => {
                // 6:u32 𝑥:tableidx 𝑒:expr et : reftype el *:vec(expr) => {type 𝑒𝑡, init el *, mode active {table 𝑥, offset 𝑒}}
                Ok(module::Element {
                    mode: module::ElementMode::Active {
                        table_index: read_table_index(bytes)?,
                        offset: consume_constant_expr(bytes)?, // TODO: confirm is constant expr && signautre type
                    },
                    ref_type: read_type(bytes)?,
                    init: read_init(bytes)?,
                })
            }
            7 => {
                // 7:u32 et : reftype el *:vec(expr) => {type 𝑒𝑡, init el *, mode declarative}
                Ok(module::Element {
                    ref_type: read_type(bytes)?,
                    mode: module::ElementMode::Declarative,
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
    bytes: &mut reader::Reader,
    elements: &mut module::ElementSection,
) -> Result<(), io::Error> {
    let count = bytes.read_vu64()?;

    for _ in 0..count {
        let element = module::Element::decode(bytes)?;
        println!("table type: {:?}", element);
        elements.push(element);
    }

    Ok(())
}

fn consume_constant_expr(bytes: &mut reader::Reader) -> Result<Vec<ast::Instruction>, io::Error> {
    self::ast::Instruction::decode_expression(bytes)
}
