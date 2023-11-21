mod parser;


use std::env;
use std::path::Path;
use std::fs::File;
use std::error::Error;
use std::io::prelude::*;


fn main() {
  use std::u32;

  let source_file_path = env::args().nth(1);

  if !source_file_path.is_some() {
    println!("Please provide a file");
    std::process::exit(1);
  }

  let path_string = String::from(source_file_path.unwrap());
  let path = Path::new(&path_string);
  let display = path.display();

  let mut file = match File::open(&path) {
      Err(why) => panic!(
          "couldn't open {}: {}"
        , display
        , Error::description(&why)
      )
    , Ok(file) => file
  };

  let mut bytes = Vec::new();
  match file.read_to_end(&mut bytes) {
      Ok(c)  => c
    , Err(e) => {
        println!("error {}", e);
        return ();
      }
  };

  let unit = parser::parse(&path_string, &mut parser::parsable_bytes::ParsableBytes::new(bytes));

  println!("{}", unit);
}
