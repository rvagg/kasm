#![no_main]

use libfuzzer_sys::fuzz_target;

use kasm::wat::Lexer;

fuzz_target!(|data: &[u8]| {
    // Convert arbitrary bytes to a string (invalid UTF-8 becomes replacement chars)
    let source = String::from_utf8_lossy(data);

    // Tokenise the input - we don't care about the result, just that it doesn't panic
    for result in Lexer::new(&source) {
        // Consume the result to ensure all code paths are exercised
        let _ = result;
    }
});
