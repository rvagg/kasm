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

    use kasm;

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
            let decoded = general_purpose::STANDARD
                .decode(&s)
                .map_err(de::Error::custom)?;
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
    enum Command {
        AssertReturn(AssertReturnCommand),
        AssertTrapCommand(AssertTrapCommand),
        AssertInvalid(AssertInvalidCommand),
        Module(ModuleCommand),
    }

    // ModuleCommand is simple enough that's a catch-all; so it must be listed last
    #[derive(Deserialize, Debug)]
    #[allow(unused)]
    struct ModuleCommand {
        #[serde(rename = "type")]
        command_type: String,
        line: i32,
        filename: String,
    }

    #[derive(Deserialize, Debug)]
    #[allow(unused)]
    struct AssertReturnCommand {
        #[serde(rename = "type")]
        command_type: String,
        line: i32,
        action: Action,
        expected: Vec<TypedValue>,
    }

    #[derive(Deserialize, Debug)]
    #[allow(unused)]
    struct AssertInvalidCommand {
        #[serde(rename = "type")]
        command_type: String,
        line: i32,
        filename: String,
        text: String,
        module_type: String,
    }

    #[derive(Deserialize, Debug)]
    #[allow(unused)]
    struct AssertTrapCommand {
        #[serde(rename = "type")]
        command_type: String,
        line: i32,
        action: Action,
        text: String,
    }

    #[derive(Deserialize, Debug)]
    #[allow(unused)]
    struct Action {
        #[serde(rename = "type")]
        action_type: String,
        field: String,
        args: Vec<TypedValue>,
    }

    #[derive(Deserialize, Debug)]
    #[allow(unused)]
    struct TypedValue {
        #[serde(rename = "type")]
        value_type: String,
        value: String,
    }

    #[derive(Deserialize)]
    struct Dump {
        header: String,
        details: String,
        disassemble: String,
    }

    // Define a parameterized test
    #[rstest]
    fn test_with_file(#[files("tests/spec/*.json")] path: PathBuf) {
        println!("testing file: {}", path.display());

        let file = path.to_string_lossy().to_string();
        let json_string =
            fs::read_to_string(&file).unwrap_or_else(|_| panic!("couldn't read file: {}", file));

        let test_data: TestData = serde_json::from_str(&json_string).unwrap();

        struct InvalidCommand<'a> {
            command: &'a AssertInvalidCommand,
            bin: &'a Vec<u8>,
            code: &'a String,
        }
        let assert_invalid_commands: Vec<_> = test_data
            .spec
            .commands
            .iter()
            .enumerate()
            .filter_map(|(index, command)| {
                if let Command::AssertInvalid(ref cmd) = command {
                    match cmd.filename.split('.').last() {
                        Some("wasm") => Some(InvalidCommand {
                            command: cmd,
                            bin: &test_data.bin[&cmd.filename].0,
                            code: &test_data.code[index],
                        }),
                        Some("wat") => None,
                        _ => panic!("Unexpected file extension in filename: {}", cmd.filename),
                    }
                } else {
                    None
                }
            })
            .collect();
        for icab in &assert_invalid_commands {
            let code_hex = icab
                .bin
                .clone()
                .iter()
                .map(|b| format!("{:02x}", b))
                .collect::<Vec<_>>()
                .join("");
            println!(
                "AssertInvalid: line = {}, filename = {}, text = {}, wasm = {}, wat = {}",
                icab.command.line, icab.command.filename, icab.command.text, code_hex, icab.code
            );
            match kasm::parser::parse(
                format!("{}/{}", icab.command.filename, icab.command.line).as_str(),
                &mut kasm::parser::reader::Reader::new(icab.bin.clone()),
            ) {
                Ok(_) => panic!("should not succeed"),
                Err(e) => assert_eq!(e.to_string(), icab.command.text),
            }
        }

        test_data.dump.iter().for_each(|(filename, dump)| {
            println!("testing to_*_string for file: {}", filename);

            let parsed = kasm::parser::parse(
                format!("{}", filename).as_str(),
                &mut kasm::parser::reader::Reader::new(test_data.bin[filename].0.clone()),
            );

            match parsed {
                Ok(_) => {}
                Err(e) => panic!("failed to parse {}: {}", filename, e),
            }

            let parsed_string = parsed
                .as_ref()
                .unwrap()
                .to_string(module::ParsedUnitFormat::Header);
            println!("parsed:\n{}", parsed_string);
            let prefix = format!("\n{}:\tfile format wasm 0x1\n\nSections:\n\n", filename);
            let header = dump
                .header
                .strip_prefix(&prefix)
                .unwrap_or_else(|| &parsed_string);
            assert_eq!(parsed_string, header);

            let parsed_string = parsed
                .as_ref()
                .unwrap()
                .to_string(module::ParsedUnitFormat::Details);
            println!("details:\n{}", parsed_string);
            let prefix = format!(
                "\n{}:\tfile format wasm 0x1\n\nSection Details:\n\n",
                filename
            );
            let details = dump
                .details
                .strip_prefix(&prefix)
                .unwrap_or_else(|| &parsed_string);
            assert_eq!(parsed_string, details);

            let parsed_string = parsed
                .as_ref()
                .unwrap()
                .to_string(module::ParsedUnitFormat::Disassemble);
            println!("disassemble:\n{}", parsed_string);
            let prefix = format!(
                "\n{}:\tfile format wasm 0x1\n\nCode Disassembly:\n\n",
                filename
            );
            let disassemble = dump
                .disassemble
                .strip_prefix(&prefix)
                .unwrap_or_else(|| &parsed_string);

            assert_eq!(parsed_string, disassemble);
        });

        /*
        test_data
            .spec
            .commands
            .iter()
            .enumerate()
            .for_each(|(index, command)| {
                let code = &test_data.code[index];
                match command {
                    Command::Module(cmd) => {
                        println!(
                            "Module: line = {}, filename = {}, code = {}",
                            cmd.line, cmd.filename, code
                        );
                    }
                    Command::AssertReturn(cmd) => {
                        println!(
                            "AssertReturn: line = {}, action type = {}, code = {}",
                            cmd.line, cmd.action.action_type, code
                        );
                    }
                    Command::AssertTrapCommand(cmd) => {
                        println!(
                            "AssertTrapCommand: line = {}, action type = {}, code = {}",
                            cmd.line, cmd.action.action_type, code
                        );
                    }
                    Command::AssertInvalid(cmd) => {
                        println!(
                            "AssertInvalid: line = {}, filename = {}, code = {}",
                            cmd.line, cmd.filename, code
                        );
                    }
                }
            });
            */
    }
}
