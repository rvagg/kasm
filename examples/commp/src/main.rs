//! CommP CLI - compute Filecoin piece commitment from stdin
//!
//! Usage: cat file.bin | kasm run commp.wasm
//! Output: 64-character hex-encoded CommP root

use std::io::{self, Read, Write};

fn main() {
    let mut hasher = commp::CommPHasher::new();
    let mut buffer = [0u8; 8192];

    // Read stdin in chunks
    loop {
        match io::stdin().read(&mut buffer) {
            Ok(0) => break,  // EOF
            Ok(n) => hasher.write(&buffer[..n]),
            Err(e) => {
                eprintln!("Error reading stdin: {}", e);
                std::process::exit(1);
            }
        }
    }

    // Output hex-encoded root
    let root = hasher.root();
    for byte in &root {
        print!("{:02x}", byte);
    }
    println!();

    // Flush stdout
    let _ = io::stdout().flush();
}
