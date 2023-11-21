mod parser;

use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let source_file_path = env::args().nth(1).ok_or("Please provide a file")?;
    let path = Path::new(&source_file_path);

    let mut file = File::open(&path)?;

    let mut bytes = Vec::new();
    file.read_to_end(&mut bytes)?;

    let unit = parser::parse(
        &source_file_path,
        &mut parser::parsable_bytes::ParsableBytes::new(bytes),
    )?;

    println!("{}", unit);

    Ok(())
}
