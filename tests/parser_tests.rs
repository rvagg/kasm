#[cfg(test)]
mod tests {
    use base64::{engine::general_purpose, Engine as _};
    use serde::de::{self, Deserializer};
    use serde::Deserialize;
    use std::collections::HashMap;

    use kasm;

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
    struct TestData {
        bin: Bin,
        spec: Spec,
        code: Vec<String>,
    }

    #[derive(Deserialize)]
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
    struct ModuleCommand {
        #[serde(rename = "type")]
        command_type: String,
        line: i32,
        filename: String,
    }

    #[derive(Deserialize, Debug)]
    struct AssertReturnCommand {
        #[serde(rename = "type")]
        command_type: String,
        line: i32,
        action: Action,
        expected: Vec<TypedValue>,
    }

    #[derive(Deserialize, Debug)]
    struct AssertInvalidCommand {
        #[serde(rename = "type")]
        command_type: String,
        line: i32,
        filename: String,
        text: String,
        module_type: String,
    }

    #[derive(Deserialize, Debug)]
    struct AssertTrapCommand {
        #[serde(rename = "type")]
        command_type: String,
        line: i32,
        action: Action,
        text: String,
    }

    #[derive(Deserialize, Debug)]
    struct Action {
        #[serde(rename = "type")]
        action_type: String,
        field: String,
        args: Vec<TypedValue>,
    }

    #[derive(Deserialize, Debug)]
    struct TypedValue {
        #[serde(rename = "type")]
        value_type: String,
        value: String,
    }

    #[test]
    fn test_json_files() {
        use std::fs;
        use std::path::Path;

        let json_files = vec!["i32.json", "i64.json"];

        for file in json_files {
            println!("testing file: {}", file);

            let path = Path::new("tests").join(file);
            let display = path.display();

            let json_string = fs::read_to_string(&path)
                .unwrap_or_else(|_| panic!("couldn't read file: {}", display));

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
                        Some(InvalidCommand {
                            command: cmd,
                            bin: &test_data.bin[&cmd.filename].0,
                            code: &test_data.code[index],
                        })
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
                    icab.command.line,
                    icab.command.filename,
                    icab.command.text,
                    code_hex,
                    icab.code
                );
                match kasm::parser::parse(
                    format!("{}/{}", icab.command.filename, icab.command.line).as_str(),
                    &mut kasm::parser::parsable_bytes::ParsableBytes::new(icab.bin.clone()),
                ) {
                    Ok(_) => panic!("should not succeed"),
                    Err(e) => assert_eq!(e.to_string(), icab.command.text),
                }
            }

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
}
