mod parser;

use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source_file_path = env::args().nth(1).ok_or("Please provide a file")?;
    let path = Path::new(&source_file_path);

    let bytes: Vec<u8> = fs::read(path)?;
    let module_registry = HashMap::new();
    let unit = parser::parse(
        &module_registry,
        &source_file_path,
        &mut parser::reader::Reader::new(bytes),
    )?;

    let args: Vec<String> = std::env::args().collect();

    if args.contains(&"--dump-header".to_string()) {
        println!("{}", unit.to_string(parser::module::ParsedUnitFormat::Header));
    } else if args.contains(&"--dump-details".to_string()) {
        println!("{}", unit.to_string(parser::module::ParsedUnitFormat::Details));
    } else if args.contains(&"--dump-disassemble".to_string()) {
        println!("{}", unit.to_string(parser::module::ParsedUnitFormat::Disassemble));
    }

    Ok(())
}
