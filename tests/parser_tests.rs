#[cfg(test)]
mod tests {
    use base64::{engine::general_purpose, Engine as _};
    use serde::de::{self, Deserializer};
    use serde::Deserialize;
    use std::collections::HashMap;

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

    #[derive(Deserialize)]
    #[serde(untagged)]
    enum Command {
        Module(ModuleCommand),
        AssertReturn(AssertReturnCommand),
        AssertTrapCommand(AssertTrapCommand),
        AssertInvalid(AssertInvalidCommand),
    }

    #[derive(Deserialize)]
    struct ModuleCommand {
        #[serde(rename = "type")]
        command_type: String,
        line: i32,
        filename: String,
    }

    #[derive(Deserialize)]
    struct AssertReturnCommand {
        #[serde(rename = "type")]
        command_type: String,
        line: i32,
        action: Action,
        expected: Vec<TypedValue>,
    }

    #[derive(Deserialize)]
    struct AssertInvalidCommand {
        #[serde(rename = "type")]
        command_type: String,
        line: i32,
        filename: String,
        text: String,
        module_type: String,
    }

    #[derive(Deserialize)]
    struct AssertTrapCommand {
        #[serde(rename = "type")]
        command_type: String,
        line: i32,
        action: Action,
        text: String,
    }

    #[derive(Deserialize)]
    struct Action {
        #[serde(rename = "type")]
        action_type: String,
        field: String,
        args: Vec<TypedValue>,
    }

    #[derive(Deserialize)]
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

            test_data.bin.iter().for_each(|(k, v)| {
                let hex_string: String = v.0.iter().map(|b| format!("{:02x}", b)).collect();
                println!("{}: {}", k, hex_string);
            });
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
        }
    }
}
