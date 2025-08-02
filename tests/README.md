# KASM Test Infrastructure

This directory contains the test infrastructure for the KASM WebAssembly parser.

## Test Files

- `parser_tests.rs` - Main test harness that runs WebAssembly spec tests
- `compile_test.mjs` - Compiles .wast files to .json format for testing
- `extract_utf8_tests.mjs` - Extracts UTF-8 validation tests to Rust unit tests
- `spec/` - Directory containing compiled test fixtures in JSON format

## Running Tests

### Standard Tests
```bash
cargo test
```

### Spec Tests Only
```bash
cargo test --test parser_tests
```

### UTF-8 Tests Only
```bash
cargo test utf8_validation
```

## Adding New Tests

1. Copy the .wast file from wasm-spec/test/core/
2. Compile it to JSON:
   ```bash
   node compile_test.mjs ../wasm-spec/test/core/testname.wast ./spec/testname.json
   ```
3. Run the tests to see failures
4. Fix parser/validation issues until tests pass

## UTF-8 Validation Tests

We extract UTF-8 validation tests from the spec and run them as Rust unit tests:

1. Compile UTF-8 test files (already done):
   ```bash
   node compile_test.mjs ../wasm-spec/test/core/utf8-custom-section-id.wast ./spec/utf8-custom-section-id.json
   node compile_test.mjs ../wasm-spec/test/core/utf8-import-field.wast ./spec/utf8-import-field.json
   node compile_test.mjs ../wasm-spec/test/core/utf8-import-module.wast ./spec/utf8-import-module.json
   ```

2. Extract and generate Rust tests:
   ```bash
   node extract_utf8_tests.mjs
   ```

This generates `src/parser/utf8_tests.rs` with 528 test cases.
