#![no_main]

use libfuzzer_sys::fuzz_target;
use std::collections::HashMap;

use kasm::parser::{self, reader::Reader};
use kasm::runtime::Store;

fuzz_target!(|data: &[u8]| {
    // First, try to parse the module
    let mut reader = Reader::new(data.to_vec());
    let module = match parser::parse(&HashMap::new(), "fuzz", &mut reader) {
        Ok(m) => m,
        Err(_) => return, // Invalid module, nothing to execute
    };

    // Try to instantiate it
    let mut store = Store::new();
    let instance_id = match store.create_instance(&module, None) {
        Ok(id) => id,
        Err(_) => return, // Instantiation failed (e.g., missing imports)
    };

    // Try to invoke each exported function with dummy arguments
    for export in &module.exports.exports {
        if let kasm::parser::module::ExportIndex::Function(_) = export.index {
            // Get function type to determine expected arguments
            let args = vec![]; // Start with no args - will likely fail but tests error handling
            let _ = store.invoke_export(instance_id, &export.name, args);
        }
    }
});
