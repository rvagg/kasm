pub mod ast;
pub mod module;
pub mod reader;
mod validate;

use std::io;
use std::u32;

use self::module::Positional;

pub fn parse(name: &str, bytes: &mut reader::Reader) -> Result<module::Module, ast::DecodeError> {
    let mut unit = module::Module::new(name);

    read_header(bytes, &mut unit.magic, &mut unit.version)?;

    let mut last_section_id = 0;

    loop {
        if !bytes.has_at_least(1) {
            //TODO: assert we got the length we expected
            break;
        }

        let sec_id = bytes
            .read_byte()
            .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "malformed section id"))?;
        if sec_id != 0 {
            // handle the reordering of datacount to be before code, make datacount 10, and shuffle 10+ up one
            let mapped_section_id = match sec_id {
                10 => 11,
                11 => 12,
                12 => 10,
                _ => sec_id,
            };
            if mapped_section_id <= last_section_id {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "unexpected content after last section",
                )
                .into());
            }
            last_section_id = mapped_section_id;
        }
        let sec_len = bytes.read_vu32()?;

        if !bytes.has_at_least(sec_len as usize) {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "length out of bounds").into());
        }
        let start_pos = bytes.pos();

        println!(
            "section #{} '{}', len = {}",
            sec_id,
            match sec_id {
                0 => "custom?",
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
                12 => "datacount",
                _ => "unknown",
            },
            sec_len
        );

        read_sections(sec_id, bytes, &mut unit, sec_len).map_err(
            |e: ast::DecodeError| match e {
                ast::DecodeError::Io(err) => match err.kind() {
                    io::ErrorKind::UnexpectedEof => io::Error::new(
                        io::ErrorKind::UnexpectedEof,
                        "unexpected end of section or function",
                    )
                    .into(),
                    _ => ast::DecodeError::Io(err),
                },
                _ => e,
            },
        )?;
        let end_pos = bytes.pos();
        let actual_len = end_pos - start_pos;
        if actual_len != sec_len as usize {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "section length mismatch, expected {}, got {}",
                    sec_len, actual_len
                ),
            )
            .into());
        }
    }

    // TODO: this check is duplicated in code section read
    if unit.functions.functions.len() != unit.code.code.len() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "function and code section have inconsistent lengths",
        )
        .into());
    }

    Ok(unit)
}

// TODO: the verbosity of this function suggest that the structure of the module
// needs to be rethought
fn read_sections(
    sec_id: u8,
    bytes: &mut reader::Reader,
    unit: &mut module::Module,
    section_len: u32,
) -> Result<(), ast::DecodeError> {
    let start_pos: usize = bytes.pos();
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
            read_section_global(bytes, &mut unit.globals, &unit.imports)?;
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
            read_section_elements(
                bytes,
                &unit.imports,
                &unit.table,
                &mut unit.elements,
                (unit.imports.function_count() + unit.functions.functions.len()) as u32,
            )?;
            &mut unit.elements as &mut dyn module::Positional
        }
        10 => {
            read_section_code(bytes, unit)?;
            &mut unit.code as &mut dyn module::Positional
        }
        11 => {
            read_section_data(
                bytes,
                &unit.imports,
                &mut unit.data,
                if unit.data_count.has_position() {
                    Some(unit.data_count.count)
                } else {
                    None
                },
                unit.memory.memory.len() as u32,
            )?;
            &mut unit.data as &mut dyn module::Positional
        }
        12 => {
            read_section_datacount(bytes, &mut unit.data_count)?;
            &mut unit.data_count as &mut dyn module::Positional
        }
        0 => {
            let custom = read_section_custom(bytes, section_len, &mut unit.custom)?;
            custom as &mut dyn module::Positional
        }
        _ => return Err(io::Error::new(io::ErrorKind::InvalidData, "malformed section id").into()),
    };

    if bytes.pos() != (start_pos + section_len as usize) {
        return Err(io::Error::new(io::ErrorKind::InvalidData, "section size mismatch").into());
    }

    let end = bytes.pos();
    positional.set_position(start_pos as u32, end as u32);

    Ok(())
}

fn read_header(
    bytes: &mut reader::Reader,
    magic: &mut u32,
    version: &mut u32,
) -> Result<(), io::Error> {
    *magic = bytes.read_u32().and_then(|value| {
        if value == 0x6d736100 {
            Ok(value)
        } else {
            Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "magic header not detected",
            ))
        }
    })?;
    *version = bytes.read_u32().and_then(|value| {
        if value == 0x1 {
            Ok(value)
        } else {
            Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "unknown binary version",
            ))
        }
    })?;
    println!("magic={}, version={}", magic, version);
    Ok(())
}

/* SECTION READERS ************************************************/

fn read_value_types_list(bytes: &mut reader::Reader) -> Result<Vec<module::ValueType>, io::Error> {
    let mut rt: Vec<module::ValueType> = vec![];
    let count = bytes.read_vu32()?;
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
    let count = bytes.read_vu32()?;

    for ii in 0..count {
        if bytes.read_byte()? != 0x60 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                // NOTE: binary-leb128.wast asserts this, for some reason, by pushing through a 0xe0 byte
                // and expecting this error string, however the spec says to expect an 0x60 byte always
                "integer representation too long",
                // "expected 0x60 to lead function type, got 0x{:02x} for func #{} / #{}", byt, ii, count
            ));
        }
        let parameters = read_value_types_list(bytes)?;
        let return_types = read_value_types_list(bytes)?;

        println!("type #{}: {:?} -> {:?}", ii, parameters, return_types);
        types.push(module::FunctionType {
            parameters: parameters,
            return_types: return_types,
        });
    }

    Ok(())
}

fn read_section_import(
    bytes: &mut reader::Reader,
    imports: &mut module::ImportSection,
) -> Result<(), ast::DecodeError> {
    let count = bytes.read_vu32()?;

    for _ in 0..count {
        let import = module::Import::decode(bytes)?;
        imports.push(import);
    }

    Ok(())
}

impl module::Import {
    pub fn decode(reader: &mut reader::Reader) -> Result<module::Import, ast::DecodeError> {
        let module = reader.read_string()?;
        let name = reader.read_string()?;
        let external_kind = module::ExternalKind::decode(reader)?;
        println!("import: {}::{} {:?}", module, name, external_kind);
        Ok(module::Import {
            external_kind,
            module,
            name,
        })
    }
}

fn read_section_function(
    bytes: &mut reader::Reader,
    functions: &mut module::FunctionSection,
    types: &module::TypeSection,
) -> Result<(), io::Error> {
    let count = bytes.read_vu32()?;

    for _ in 0..count {
        let ftype_index = bytes.read_vu32()?;
        if ftype_index >= types.len() as u32 {
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
) -> Result<(), ast::DecodeError> {
    let count = bytes.read_vu32()?;

    for _ in 0..count {
        let limits = module::Limits::decode(bytes)?;
        memory.push(module::Memory {
            memory_type: limits,
        })
    }

    println!("memory: {:?}", memory);

    Ok(())
}

fn read_section_export(
    bytes: &mut reader::Reader,
    exports: &mut module::ExportSection,
) -> Result<(), io::Error> {
    let count = bytes.read_vu32()?;

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
    module: &mut module::Module,
) -> Result<(), ast::DecodeError> {
    let count = bytes.read_vu32()?;

    // TODO: this check is duplicated after the section read loop
    if count != module.functions.functions.len() as u32 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "function and code section have inconsistent lengths",
        )
        .into());
    }

    for ii in 0..count {
        let size = bytes.read_vu32()? as usize;
        let start_pos = bytes.pos();
        let end_pos = start_pos + size;
        let locals_count = bytes.read_vu32()?;

        let mut nts = Vec::new();
        for _ in 0..locals_count {
            let local_n = bytes.read_vu32()?;
            let b = bytes.read_byte()?;
            let value = module::ValueType::decode(b)
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
            nts.push((local_n, value));
        }

        let locals = module::Locals::new(nts);
        if locals.len() >= u64::from(u32::MAX) {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "too many locals").into());
        }

        let instructions: Vec<ast::Instruction> =
            ast::Instruction::decode_function(&module, &locals, module.code.len() as u32, bytes)
                .map_err(|err| {
                    if bytes.pos() > end_pos {
                        io::Error::new(io::ErrorKind::InvalidData, "END opcode expected").into()
                    } else if ii == count - 1 && bytes.pos() == end_pos {
                        match err {
                            ast::DecodeError::Validation(_) => err,
                            _ => io::Error::new(
                                io::ErrorKind::InvalidData,
                                "unexpected end of section or function",
                            )
                            .into(),
                        }
                    } else {
                        err
                    }
                })?;
        if bytes.pos() < end_pos {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "unexpected end of section or function",
            )
            .into());
        }
        let function_body = module::FunctionBody {
            locals,
            // body: body,
            instructions,
            position: module::SectionPosition {
                start: start_pos as u32,
                end: (start_pos + size) as u32,
            },
        };
        module.code.code.push(function_body);
    }

    Ok(())
}

impl module::Data {
    pub fn decode(
        bytes: &mut reader::Reader,
        imports: &module::ImportSection,
        memory_count: u32,
    ) -> Result<Self, ast::DecodeError> {
        let mut mode = module::DataMode::Passive;
        let typ = bytes.read_vu32()?;
        match typ {
            0 => {
                mode = module::DataMode::Active {
                    memory_index: 0,
                    offset: ast::Instruction::decode_constant_expression(
                        bytes,
                        &imports,
                        module::ValueType::I32,
                    )?,
                };
            }
            1 => {} // nothing else needed
            2 => {
                mode = module::DataMode::Active {
                    memory_index: bytes.read_vu32()?,
                    offset: ast::Instruction::decode_constant_expression(
                        bytes,
                        &imports,
                        module::ValueType::I32,
                    )?,
                };
            }
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("unknown data type: 0x{:02x}", typ),
            ))?,
        };

        if let module::DataMode::Active {
            memory_index,
            ref offset,
        } = mode
        {
            if memory_index != 0 || imports.memory_count() as u32 + memory_count == 0 {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("unknown memory {}", memory_index),
                )
                .into());
            }
            // we expect a constant and an End
            if offset.len() != 2 {
                return Err(io::Error::new(io::ErrorKind::InvalidData, "type mismatch").into());
            }
        }

        Ok(module::Data {
            init: bytes.read_u8vec()?, // TODO: this needs to trigger a 'unexpected end of section or function' rather than 'length out of bounds'
            mode: mode,
        })
    }
}

fn read_section_data(
    bytes: &mut reader::Reader,
    imports: &module::ImportSection,
    datas: &mut module::DataSection,
    data_count: Option<u32>,
    memory_count: u32,
) -> Result<(), ast::DecodeError> {
    let count = bytes.read_vu32()?;

    if let Some(expected_count) = data_count {
        if expected_count != count {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "data count and data section have inconsistent lengths",
            )
            .into());
        }
    }

    for _ in 0..count {
        let data = module::Data::decode(bytes, imports, memory_count)?;
        println!("data type: {:?}", data);
        datas.data.push(data);
    }

    Ok(())
}

fn read_section_custom<'a>(
    bytes: &mut reader::Reader,
    read_len: u32,
    customs: &'a mut Vec<module::CustomSection>,
) -> Result<&'a mut module::CustomSection, ast::DecodeError> {
    let mut custom = module::CustomSection::new();
    let start_pos = bytes.pos();
    custom.name = bytes.read_string()?;
    if bytes.pos() >= start_pos && read_len as usize >= (bytes.pos() - start_pos) as usize {
        custom.data = bytes.read_bytes(read_len as usize - (bytes.pos() - start_pos) as usize)?;
        customs.push(custom);
        if let Some(last_custom) = customs.last_mut() {
            Ok(last_custom)
        } else {
            panic!("custom section not found");
        }
    } else {
        Err(io::Error::new(io::ErrorKind::UnexpectedEof, "unexpected end").into())
    }
}

fn read_section_datacount(
    bytes: &mut reader::Reader,
    data_count: &mut module::DataCountSection,
) -> Result<(), ast::DecodeError> {
    data_count.count = bytes.read_vu32()?;
    Ok(())
}

impl module::ExternalKind {
    pub fn decode(bytes: &mut reader::Reader) -> Result<module::ExternalKind, ast::DecodeError> {
        let byte = bytes.read_byte()?;
        match byte {
            0x00 => {
                let typeidx = bytes.read_vu32()?;
                Ok(module::ExternalKind::Function(typeidx))
            }
            0x01 => {
                let tabletype = module::TableType::decode(bytes)?;
                Ok(module::ExternalKind::Table(tabletype))
            }
            0x02 => {
                let memtype = module::Limits::decode(bytes)?;
                Ok(module::ExternalKind::Memory(memtype))
            }
            0x03 => {
                let globaltype = module::GlobalType::decode(bytes)?;
                Ok(module::ExternalKind::Global(globaltype))
            }
            _ => {
                return Err(
                    io::Error::new(io::ErrorKind::InvalidData, "malformed import kind").into(),
                );
            }
        }
    }
}

impl module::GlobalType {
    pub fn decode(bytes: &mut reader::Reader) -> Result<Self, io::Error> {
        let value_type = module::ValueType::decode(bytes.read_byte()?)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
        let mutable = match bytes.read_byte()? {
            0x00 => false,
            0x01 => true,
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "invalid global type mutable flag",
            ))?,
        };
        Ok(module::GlobalType {
            value_type: value_type,
            mutable: mutable,
        })
    }
}

impl module::Global {
    pub fn decode(
        bytes: &mut reader::Reader,
        imports: &module::ImportSection,
    ) -> Result<Self, ast::DecodeError> {
        let global_type = module::GlobalType::decode(bytes)?;
        Ok(module::Global {
            global_type,
            init: ast::Instruction::decode_constant_expression(
                bytes,
                &imports,
                global_type.value_type,
            )?,
        })
    }
}

fn read_section_global(
    bytes: &mut reader::Reader,
    globals: &mut module::GlobalSection,
    imports: &module::ImportSection,
) -> Result<(), ast::DecodeError> {
    let count = bytes.read_vu32()?;

    for _ in 0..count {
        let data = module::Global::decode(bytes, imports)?;
        println!("global: {:?}", data);
        globals.globals.push(data);
    }

    Ok(())
}

impl module::RefType {
    pub fn decode(bytes: &mut reader::Reader) -> Result<Self, ast::DecodeError> {
        let byte = bytes.read_byte()?;
        match byte {
            0x70 => Ok(module::RefType::FuncRef),
            0x6f => Ok(module::RefType::ExternRef),
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("unknown ref type: 0x{:02x}", byte),
            )
            .into()),
        }
    }
}

impl module::Limits {
    pub fn decode(bytes: &mut reader::Reader) -> Result<Self, ast::DecodeError> {
        let has_max = bytes.read_vu1()?;
        Ok(module::Limits {
            min: bytes.read_vu32()?,
            max: if has_max {
                bytes.read_vu32()?
            } else {
                u32::MAX
            },
        })
    }
}

impl module::TableType {
    pub fn decode(bytes: &mut reader::Reader) -> Result<Self, ast::DecodeError> {
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
) -> Result<(), ast::DecodeError> {
    let count = bytes.read_vu32()?;

    for _ in 0..count {
        let table_type = module::TableType::decode(bytes)?;
        println!("table type: {:?}", table_type);
        table.tables.push(table_type);
    }

    Ok(())
}

fn read_section_start(
    bytes: &mut reader::Reader,
    start: &mut module::StartSection,
) -> Result<(), io::Error> {
    start.start = bytes.read_vu32()?;
    Ok(())
}

impl module::Element {
    pub fn decode(
        bytes: &mut reader::Reader,
        imports: &module::ImportSection,
        table: &module::TableSection,
        function_count: u32,
    ) -> Result<Self, ast::DecodeError> {
        let read_type = |bytes: &mut reader::Reader| -> Result<module::RefType, io::Error> {
            let byte = bytes.read_byte()?;
            match byte {
                0x70 => Ok(module::RefType::FuncRef),
                0x6f => Ok(module::RefType::ExternRef),
                _ => Err(
                    io::Error::new(io::ErrorKind::InvalidData, "malformed reference type").into(),
                ),
            }
        };
        let read_element_kind = |bytes: &mut reader::Reader| -> Result<module::RefType, io::Error> {
            let byte = bytes.read_byte()?;
            match byte {
                0x00 => Ok(module::RefType::FuncRef),
                _ => Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("unknown element element kind: 0x{:02x}", byte),
                )
                .into()),
            }
        };
        let read_table_index = |bytes: &mut reader::Reader| -> Result<u32, ast::DecodeError> {
            bytes.read_vu32().map_err(|e| e.into())
        };
        let read_init = |bytes: &mut reader::Reader,
                         return_type: module::ValueType|
         -> Result<Vec<Vec<ast::Instruction>>, ast::DecodeError> {
            let count = bytes.read_vu32()?;
            println!("read_init count: {}", count);
            let mut init: Vec<Vec<ast::Instruction>> = vec![];
            for _ in 0..count {
                init.push(ast::Instruction::decode_constant_expression(
                    bytes,
                    imports,
                    return_type,
                )?);
            }
            Ok(init)
        };
        let read_func_indexes = |bytes: &mut reader::Reader| -> Result<Vec<u32>, io::Error> {
            let count = bytes.read_vu32()?;
            let mut func_indexes: Vec<u32> = vec![];
            for _ in 0..count {
                let func_index = bytes.read_vu32()?;
                if func_index >= function_count {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "unknown function",
                    ));
                }
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
                                0,
                                0,
                            ),
                            ast::Instruction::new(
                                ast::InstructionType::End,
                                ast::InstructionData::SimpleInstruction {
                                    subopcode_bytes: Vec::new(),
                                },
                                0,
                                0,
                            ),
                        ]
                    })
                    .collect())
            };

        let typ = bytes.read_vu32()?;
        println!("element type: 0x{:02x}", typ);
        let element = match typ {
            0 => {
                // 0:u32 𝑒:expr 𝑦*:vec(funcidx) => {type funcref, init ((ref.func 𝑦) end)*, mode active {table 0, offset 𝑒}}
                module::Element {
                    flags: typ,
                    ref_type: module::RefType::FuncRef,
                    mode: module::ElementMode::Active {
                        table_index: 0,
                        offset: ast::Instruction::decode_constant_expression(
                            bytes,
                            imports,
                            module::ValueType::I32,
                        )?,
                    },
                    init: read_func_index_init(bytes)?,
                }
            }
            1 => {
                // 1:u32 et : elemkind 𝑦*:vec(funcidx) => {type et, init ((ref.func 𝑦) end)*, mode passive}
                module::Element {
                    flags: typ,
                    ref_type: read_element_kind(bytes)?,
                    mode: module::ElementMode::Passive,
                    init: read_func_index_init(bytes)?,
                }
            }
            2 => {
                // 2:u32 𝑥:tableidx 𝑒:expr et : elemkind 𝑦*:vec(funcidx) => {type et, init ((ref.func 𝑦) end)*, mode active {table 𝑥, offset 𝑒}}
                module::Element {
                    flags: typ,
                    mode: module::ElementMode::Active {
                        table_index: read_table_index(bytes)?,
                        offset: ast::Instruction::decode_constant_expression(
                            bytes,
                            imports,
                            module::ValueType::I32,
                        )?,
                    },
                    ref_type: read_element_kind(bytes)?,
                    init: read_func_index_init(bytes)?,
                }
            }
            3 => {
                // 3:u32 et : elemkind 𝑦*:vec(funcidx) => {type et, init ((ref.func 𝑦) end)*, mode declarative}
                module::Element {
                    flags: typ,
                    ref_type: read_element_kind(bytes)?,
                    mode: module::ElementMode::Declarative,
                    init: read_func_index_init(bytes)?,
                }
            }
            4 => {
                // 4:u32 𝑒:expr el *:vec(expr) => {type funcref, init el *, mode active {table 0, offset 𝑒}}
                module::Element {
                    flags: typ,
                    ref_type: module::RefType::FuncRef,
                    mode: module::ElementMode::Active {
                        table_index: 0,
                        offset: ast::Instruction::decode_constant_expression(
                            bytes,
                            imports,
                            module::ValueType::I32,
                        )?,
                    },
                    init: read_init(bytes, module::ValueType::FuncRef)?,
                }
            }
            5 => {
                // 5:u32 et : reftype el *:vec(expr) => {type 𝑒𝑡, init el *, mode passive}
                let ref_type = read_type(bytes)?;
                let init = read_init(bytes, ref_type.into())?;
                module::Element {
                    flags: typ,
                    ref_type,
                    mode: module::ElementMode::Passive,
                    init,
                }
            }
            6 => {
                // 6:u32 𝑥:tableidx 𝑒:expr et : reftype el *:vec(expr) => {type 𝑒𝑡, init el *, mode active {table 𝑥, offset 𝑒}}
                let table_index = read_table_index(bytes)?;
                let offset = ast::Instruction::decode_constant_expression(
                    bytes,
                    imports,
                    module::ValueType::I32,
                )?;
                let ref_type = read_type(bytes)?;
                let init = read_init(bytes, ref_type.into())?;
                module::Element {
                    flags: typ,
                    mode: module::ElementMode::Active {
                        table_index,
                        offset,
                    },
                    ref_type,
                    init,
                }
            }
            7 => {
                // 7:u32 et : reftype el *:vec(expr) => {type 𝑒𝑡, init el *, mode declarative}
                let ref_type = read_type(bytes)?;
                let init = read_init(bytes, ref_type.into())?;
                module::Element {
                    flags: typ,
                    ref_type,
                    mode: module::ElementMode::Declarative,
                    init,
                }
            }
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("unknown element type: 0x{:02x}", typ),
                )
                .into());
            }
        };
        match element.mode {
            module::ElementMode::Active { table_index, .. } => {
                let ref_type: Option<&module::TableType> = if table_index < imports.table_count() as u32 {
                    imports.get_table(table_index)
                } else if (table_index - imports.table_count() as u32) < table.tables.len() as u32 {
                    table
                        .tables
                        .get((table_index - imports.table_count() as u32) as usize)
                } else {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        format!("unknown table {}", table_index),
                    )
                    .into());
                };
                if let Some(ref_type) = ref_type {
                    if element.ref_type != ref_type.ref_type {
                        return Err(io::Error::new(
                            io::ErrorKind::InvalidData,
                            "element type mismatch",
                        )
                        .into());
                    }
                }
            }
            _ => {}
        };
        Ok(element)
    }
}

fn read_section_elements(
    bytes: &mut reader::Reader,
    imports: &module::ImportSection,
    table: &module::TableSection,
    elements: &mut module::ElementSection,
    function_count: u32,
) -> Result<(), ast::DecodeError> {
    let count = bytes.read_vu32()?;

    for _ in 0..count {
        let element = module::Element::decode(bytes, imports, table, function_count)?;
        println!("element type: {:?}", element);
        elements.push(element);
    }

    Ok(())
}
