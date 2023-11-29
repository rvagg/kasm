mod parser;

use std::env;
use std::fs;
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source_file_path = env::args().nth(1).ok_or("Please provide a file")?;
    let path = Path::new(&source_file_path);

    let bytes = fs::read(&path)?;

    let unit = parser::parse(&source_file_path, &mut parser::reader::Reader::new(bytes))?;

    println!("{}", unit);

    Ok(())
}
