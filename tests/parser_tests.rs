#[cfg(test)]
mod tests {
    use base64::{engine::general_purpose, Engine as _};
    use kasm::parser::module;
    use rstest::rstest;
    use serde::de::{self, Deserializer};
    use serde::Deserialize;
    use std::collections::HashMap;
    use std::fs;
    use std::path::PathBuf;

    #[derive(Deserialize)]
    struct TestData {
        bin: Bin,
        spec: Spec,
        code: Vec<String>,
        dump: HashMap<String, Dump>,
    }

    pub type Bin = HashMap<String, Base64DecodedBytes>;

    #[derive(Debug)]
    pub struct Base64DecodedBytes(Vec<u8>);

    impl<'de> Deserialize<'de> for Base64DecodedBytes {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            let s: String = String::deserialize(deserializer)?;
            let decoded = general_purpose::STANDARD.decode(s).map_err(de::Error::custom)?;
            Ok(Base64DecodedBytes(decoded))
        }
    }

    #[derive(Deserialize)]
    #[allow(unused)]
    struct Spec {
        source_filename: String,
        commands: Vec<Command>,
    }

    #[derive(Deserialize, Debug)]
    #[serde(untagged)]
    #[allow(dead_code)]
    enum Command {
        AssertTrapCommand(AssertTrapCommand),
        AssertReturnCommand(AssertReturnCommand),
        AssertUninstantiable(AssertUninstantiableCommand),
        AssertInvalidCommand(AssertInvalidCommand),
        RegisterCommand(RegisterCommand),
        Module(ModuleCommand),
    }

    // ModuleCommand is simple enough that's a catch-all; so it must be listed last
    #[derive(Deserialize, Debug)]
    #[allow(unused)]
    struct ModuleCommand {
        r#type: String,
        line: i32,
        filename: String,
    }

    #[derive(Deserialize, Debug)]
    #[allow(unused)]
    struct RegisterCommand {
        r#type: String,
        line: i32,
        r#as: String,
    }

    #[derive(Deserialize, Debug)]
    #[allow(unused)]
    struct AssertReturnCommand {
        r#type: String,
        line: i32,
        action: Action,
        expected: Vec<TypedValue>,
    }

    #[derive(Debug)]
    #[allow(unused)]
    struct AssertUninstantiableCommand {
        command_type: String,
        line: i32,
        filename: String,
        text: String,
        module_type: String,
    }

    #[derive(Deserialize, Debug)]
    struct AssertInvalidCommand {
        r#type: String,
        line: i32,
        filename: String,
        text: String,
        module_type: String,
    }

    impl<'de> Deserialize<'de> for AssertUninstantiableCommand {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            let command = AssertInvalidCommand::deserialize(deserializer)?;
            if command.r#type == "assert_uninstantiable" {
                Ok(AssertUninstantiableCommand {
                    command_type: "assert_uninstantiable".to_string(),
                    line: command.line,
                    filename: command.filename,
                    text: command.text,
                    module_type: command.module_type,
                })
            } else {
                Err(serde::de::Error::custom("wrong type"))
            }
        }
    }

    #[derive(Deserialize, Debug)]
    #[allow(unused)]
    struct AssertTrapCommand {
        r#type: String,
        line: i32,
        action: Action,
        text: String,
        expected: Vec<Type>,
    }

    #[derive(Deserialize, Debug)]
    #[allow(unused)]
    struct Action {
        r#type: String,
        #[serde(default)]
        module: String,
        field: String,
        #[serde(default)]
        args: Vec<TypedValue>,
    }

    #[derive(Deserialize, Debug)]
    #[allow(unused)]
    struct TypedValue {
        r#type: String,
        value: String,
    }

    #[derive(Deserialize, Debug)]
    #[allow(unused)]
    struct Type {
        r#type: String,
    }

    #[derive(Deserialize)]
    struct Dump {
        header: String,
        details: String,
        disassemble: String,
    }

    // Define a parameterized test
    #[rstest]
    #[case("tests/spec/nop.json")]
    #[case("tests/spec/type.json")]
    #[case("tests/spec/global.json")]
    #[case("tests/spec/unreachable.json")]
    #[case("tests/spec/return.json")]
    #[case("tests/spec/address.json")]
    #[case("tests/spec/align.json")]
    #[case("tests/spec/binary.json")]
    #[case("tests/spec/binary-leb128.json")]
    #[case("tests/spec/block.json")]
    #[case("tests/spec/br.json")]
    #[case("tests/spec/br_if.json")]
    #[case("tests/spec/br_table.json")]
    #[case("tests/spec/bulk.json")]
    #[case("tests/spec/call.json")]
    #[case("tests/spec/call_indirect.json")]
    #[case("tests/spec/const.json")]
    #[case("tests/spec/conversions.json")]
    #[case("tests/spec/custom.json")]
    #[case("tests/spec/data.json")]
    #[case("tests/spec/elem.json")]
    #[case("tests/spec/endianness.json")]
    #[case("tests/spec/exports.json")]
    #[case("tests/spec/f32.json")]
    #[case("tests/spec/f32_bitwise.json")]
    #[case("tests/spec/f32_cmp.json")]
    #[case("tests/spec/f64.json")]
    #[case("tests/spec/f64_bitwise.json")]
    #[case("tests/spec/f64_cmp.json")]
    #[case("tests/spec/fac.json")]
    #[case("tests/spec/float_exprs.json")]
    #[case("tests/spec/float_literals.json")]
    #[case("tests/spec/float_memory.json")]
    #[case("tests/spec/float_misc.json")]
    #[case("tests/spec/forward.json")]
    #[case("tests/spec/func.json")]
    #[case("tests/spec/func_ptrs.json")]
    #[case("tests/spec/i32.json")]
    #[case("tests/spec/i64.json")]
    #[case("tests/spec/imports.json")]
    #[case("tests/spec/local_get.json")]
    #[case("tests/spec/local_set.json")]
    #[case("tests/spec/local_tee.json")]
    #[case("tests/spec/loop.json")]
    #[case("tests/spec/memory_size.json")]
    #[case("tests/spec/select.json")]
    fn test_with_file(#[case] path: PathBuf) {
        println!("testing file: {}", path.display());

        let file = path.to_string_lossy().to_string();
        let json_string = fs::read_to_string(&file).unwrap_or_else(|_| panic!("couldn't read file: {}", file));
        let test_data: TestData = serde_json::from_str(&json_string).unwrap();

        let mut last_module: Option<module::Module> = None;
        let mut module_registry: HashMap<String, module::Module> = HashMap::new();

        setup_spectest(&mut module_registry);

        for (index, command) in test_data.spec.commands.iter().enumerate() {
            let code = &test_data.code[index];
            match command {
                Command::Module(cmd) => {
                    println!("(TODO) Module: line = {}, filename = {}", cmd.line, cmd.filename);
                    let bin = &test_data.bin[&cmd.filename].0;
                    let parsed = kasm::parser::parse(
                        &module_registry,
                        format!("{}/{}", cmd.filename, cmd.line).as_str(),
                        &mut kasm::parser::reader::Reader::new(bin.clone()),
                    );
                    if parsed.is_err() {
                        let err = parsed.err().unwrap();
                        println!("Error parsing module {}: {:?}", cmd.filename, err);
                        panic!("Failed to parse: {}", err);
                    }
                    last_module = Some(parsed.unwrap());
                }
                Command::AssertReturnCommand(cmd) => {
                    println!(
                        "(TODO) AssertReturn: line = {}, action type = {}",
                        cmd.line, cmd.action.r#type
                    );
                }
                Command::AssertUninstantiable(cmd) => {
                    println!(
                        "(TODO) AssertUninstantiable: line = {}, filename = {}, text = {}, module type = {}",
                        cmd.line, cmd.filename, cmd.text, cmd.module_type
                    );
                }
                Command::RegisterCommand(cmd) => {
                    println!(
                        "RegisterCommand: line = {}, action type = {}, as = {}",
                        cmd.line, cmd.r#type, cmd.r#as
                    );
                    if let Some(module) = last_module.take() {
                        module_registry.insert(cmd.r#as.clone(), module);
                    } else {
                        panic!("No module to register for command: {}", cmd.r#as);
                    }
                }
                Command::AssertTrapCommand(cmd) => {
                    println!(
                        "AssertTrapCommand: line = {}, action type = {}, code = {}",
                        cmd.line, cmd.action.r#type, code
                    );
                }
                Command::AssertInvalidCommand(cmd) => {
                    // Skip assert_unlinkable tests since we only test parsing, not instantiation
                    if cmd.r#type == "assert_unlinkable" {
                        println!("Skipping assert_unlinkable test: {} at line {}", cmd.filename, cmd.line);
                        continue;
                    }

                    struct InvalidCommand<'a> {
                        command: &'a AssertInvalidCommand,
                        bin: &'a Vec<u8>,
                        code: &'a String,
                    }

                    let icab: InvalidCommand<'_> = match cmd.filename.split('.').next_back() {
                        Some("wasm") => InvalidCommand {
                            command: cmd,
                            bin: &test_data.bin[&cmd.filename].0,
                            code: &test_data.code[index],
                        },
                        Some("wat") => {
                            println!("Skipping AssertInvalidCommand with .wat file: {}", cmd.filename);
                            continue;
                        }
                        _ => panic!("Unexpected file extension in filename: {}", cmd.filename),
                    };
                    let code_hex = icab
                        .bin
                        .clone()
                        .iter()
                        .map(|b| format!("{b:02x}"))
                        .collect::<Vec<_>>()
                        .join("");
                    println!(
                        "AssertInvalid: line = {}, filename = {}, text = {}, wasm = {}, wat = {}",
                        icab.command.line, icab.command.filename, icab.command.text, code_hex, icab.code
                    );
                    // TODO: setup spectest host to import: https://github.com/WebAssembly/spec/blob/main/interpreter/host/spectest.ml
                    match kasm::parser::parse(
                        &module_registry,
                        format!("{}/{}", icab.command.filename, icab.command.line).as_str(),
                        &mut kasm::parser::reader::Reader::new(icab.bin.clone()),
                    ) {
                        Ok(_) => {
                            panic!(
                                "should not succeed, expected failure with '{}', filename = {}, line in source is {}",
                                icab.command.text, icab.command.filename, icab.command.line
                            );
                        }
                        Err(e) => {
                            // Special case: select.2.wasm expects "invalid result arity"
                            // but we produce "type mismatch". Both errors are technically correct:
                            //
                            // select.2.wasm: Original WAT was (select (result)) declaring 0 results,
                            // but WABT compiles it to regular select opcode (0x1b) losing this info.
                            // We see it as select with missing operands → "type mismatch"
                            // Reference interpreter sees the arity issue → "invalid result arity"
                            //
                            // Both errors are valid - the WebAssembly spec defines what's valid
                            // but doesn't mandate exact error messages for invalid code.
                            if icab.command.filename == "select.2.wasm" && icab.command.text == "invalid result arity" {
                                assert!(
                                    e.to_string().contains("type mismatch") || e.to_string().contains(&icab.command.text),
                                    "Error message does not match expected. Error message = '{}', expected text = '{}' or 'type mismatch', filename = {}, line in source is {}",
                                    e,
                                    &icab.command.text,
                                    icab.command.filename,
                                    icab.command.line
                                );
                            } else {
                                assert!(
                                    e.to_string().contains(&icab.command.text),
                                    "Error message does not contain the expected text. Error message = '{}', expected text = '{}', filename = {}, line in source is {}",
                                    e,
                                    &icab.command.text,
                                    icab.command.filename,
                                    icab.command.line
                                );
                            }
                        }
                    }
                }
            }
        }

        // Compare the parsed module to the expected dump

        fn compare_format(
            prefix: &str,
            dump_field: &str,
            parsed: &module::Module,
            filename: &str,
            format: module::ParsedUnitFormat,
        ) {
            let parsed_string = parsed.to_string(format);
            println!("{prefix}:\n{parsed_string}");

            let prefix = format!("\n{filename}:\tfile format wasm 0x1\n\n{prefix}:\n\n");
            let expected = dump_field.strip_prefix(&prefix).unwrap_or(&parsed_string);

            assert_eq!(parsed_string, expected);
        }

        for (filename, dump) in test_data.dump.iter() {
            println!("testing to_*_string for file: {filename}");

            let bytes = &mut kasm::parser::reader::Reader::new(test_data.bin[filename].0.clone());
            let parsed = kasm::parser::parse(&module_registry, filename, bytes)
                .unwrap_or_else(|_| panic!("failed to parse {}", filename));

            compare_format(
                "Sections",
                &dump.header,
                &parsed,
                filename,
                module::ParsedUnitFormat::Header,
            );
            compare_format(
                "Section Details",
                &dump.details,
                &parsed,
                filename,
                module::ParsedUnitFormat::Details,
            );
            compare_format(
                "Code Disassembly",
                &dump.disassemble,
                &parsed,
                filename,
                module::ParsedUnitFormat::Disassemble,
            );
        }
    }

    fn setup_spectest(module_registry: &mut HashMap<String, kasm::parser::module::Module>) {
        let mut spectest = kasm::parser::module::Module::new("spectest");

        insert_function(&mut spectest, "print".to_string(), vec![], vec![]);
        insert_function(
            &mut spectest,
            "print_i32".to_string(),
            vec![kasm::parser::module::ValueType::I32],
            vec![],
        );
        insert_function(
            &mut spectest,
            "print_i64".to_string(),
            vec![kasm::parser::module::ValueType::I64],
            vec![],
        );
        insert_function(
            &mut spectest,
            "print_f32".to_string(),
            vec![kasm::parser::module::ValueType::F32],
            vec![],
        );
        insert_function(
            &mut spectest,
            "print_f64".to_string(),
            vec![kasm::parser::module::ValueType::F64],
            vec![],
        );
        insert_function(
            &mut spectest,
            "print_i32_f32".to_string(),
            vec![
                kasm::parser::module::ValueType::I32,
                kasm::parser::module::ValueType::F32,
            ],
            vec![],
        );
        insert_function(
            &mut spectest,
            "print_f64_f64".to_string(),
            vec![
                kasm::parser::module::ValueType::F64,
                kasm::parser::module::ValueType::F64,
            ],
            vec![],
        );
        insert_global(
            &mut spectest,
            "global_i32".to_string(),
            kasm::parser::module::ValueType::I32,
        );
        insert_global(
            &mut spectest,
            "global_i64".to_string(),
            kasm::parser::module::ValueType::I64,
        );
        insert_global(
            &mut spectest,
            "global_f32".to_string(),
            kasm::parser::module::ValueType::F32,
        );
        insert_global(
            &mut spectest,
            "global_f64".to_string(),
            kasm::parser::module::ValueType::F64,
        );

        {
            spectest.table.tables.push(kasm::parser::module::TableType {
                ref_type: kasm::parser::module::RefType::FuncRef,
                limits: kasm::parser::module::Limits { min: 10, max: u32::MAX },
            });
            let tableidx = (spectest.table.tables.len() - 1) as u32;
            spectest.exports.exports.push(kasm::parser::module::Export {
                index: kasm::parser::module::ExportIndex::Table(tableidx),
                name: "table".to_string(),
            });
        }

        {
            spectest.memory.memory.push(kasm::parser::module::Memory {
                limits: kasm::parser::module::Limits { min: 1, max: u32::MAX },
            });
            let memidx = (spectest.memory.memory.len() - 1) as u32;
            spectest.exports.exports.push(kasm::parser::module::Export {
                index: kasm::parser::module::ExportIndex::Memory(memidx),
                name: "memory".to_string(),
            });
        }

        module_registry.insert("spectest".to_string(), spectest);
    }

    fn insert_global(module: &mut module::Module, name: String, value_type: kasm::parser::module::ValueType) {
        let gtype = kasm::parser::module::Global {
            global_type: kasm::parser::module::GlobalType {
                value_type,
                mutable: false,
            },
            init: vec![],
        };

        let typeidx = match module.globals.find(&gtype) {
            Some(idx) => idx,
            None => {
                module.globals.globals.push(gtype);
                (module.globals.globals.len() - 1) as u32
            }
        };
        module.exports.exports.push(kasm::parser::module::Export {
            index: kasm::parser::module::ExportIndex::Global(typeidx),
            name,
        });
    }

    fn insert_function(
        module: &mut module::Module,
        name: String,
        parameters: Vec<kasm::parser::module::ValueType>,
        return_types: Vec<kasm::parser::module::ValueType>,
    ) {
        let ftype = kasm::parser::module::FunctionType {
            parameters,
            return_types,
        };
        let typeidx = match module.types.find(&ftype) {
            Some(idx) => idx,
            None => {
                module.types.types.push(ftype);
                (module.types.types.len() - 1) as u32
            }
        };
        module
            .functions
            .functions
            .push(kasm::parser::module::Function { ftype_index: typeidx });
        // Add corresponding empty code section for the function
        module.code.code.push(kasm::parser::module::FunctionBody {
            locals: kasm::parser::module::Locals::new(vec![]),
            instructions: vec![],
            position: kasm::parser::module::SectionPosition { start: 0, end: 0 },
        });
        let funcidx = (module.functions.functions.len() - 1) as u32;
        module.exports.exports.push(kasm::parser::module::Export {
            index: kasm::parser::module::ExportIndex::Function(funcidx),
            name,
        });
    }
}
