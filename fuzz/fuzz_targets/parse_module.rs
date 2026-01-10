#![no_main]

use libfuzzer_sys::fuzz_target;
use std::collections::HashMap;

use kasm::parser::{self, reader::Reader};

fuzz_target!(|data: &[u8]| {
    let mut reader = Reader::new(data.to_vec());
    // We don't care about the result - we're looking for panics/crashes
    let _ = parser::parse(&HashMap::new(), "fuzz", &mut reader);
});
