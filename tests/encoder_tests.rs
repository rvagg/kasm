//! Encoder tests: unit, round-trip, and negative tests.
//!
//! Testing strategy: "encode stability". Since Module does not implement
//! PartialEq, we verify correctness by encoding a Module to bytes A,
//! parsing bytes A back to a new Module, encoding that to bytes B, and
//! asserting A == B. Deterministic encoding guarantees that byte equality
//! proves semantic equivalence through the round-trip.

#[cfg(test)]
mod tests {
    use base64::{Engine as _, engine::general_purpose};
    use kasm::{encoder, parser, wat};
    use serde::Deserialize;
    use serde::de::{self, Deserializer};
    use std::collections::HashMap;
    use std::fs;

    // =======================================================================
    // Helpers
    // =======================================================================

    /// Parses WAT source to a Module, encodes it, re-parses, re-encodes, and
    /// asserts byte-level equality (encode stability).
    fn assert_wat_round_trip(wat_source: &str) {
        let module = wat::parse(wat_source).unwrap_or_else(|e| panic!("WAT parse failed: {e}"));
        let bytes_a = encoder::encode(&module).unwrap_or_else(|e| panic!("first encode failed: {e}"));
        assert_eq!(&bytes_a[0..4], b"\0asm", "encoded bytes should start with wasm magic");

        let reparsed = parser::parse(
            &HashMap::new(),
            "round-trip",
            &mut parser::reader::Reader::new(bytes_a.clone()),
        )
        .unwrap_or_else(|e| panic!("re-parse failed: {e}"));

        let bytes_b = encoder::encode(&reparsed).unwrap_or_else(|e| panic!("second encode failed: {e}"));
        assert_eq!(bytes_a, bytes_b, "encode stability failed");
    }

    /// Parses a binary wasm module, encodes it, re-parses, re-encodes, and
    /// asserts byte-level equality.
    fn assert_binary_round_trip(wasm_bytes: &[u8]) {
        let module = parser::parse(
            &HashMap::new(),
            "binary-rt",
            &mut parser::reader::Reader::new(wasm_bytes.to_vec()),
        )
        .unwrap_or_else(|e| panic!("binary parse failed: {e}"));

        let bytes_a = encoder::encode(&module).unwrap_or_else(|e| panic!("first encode failed: {e}"));

        let reparsed = parser::parse(
            &HashMap::new(),
            "binary-rt2",
            &mut parser::reader::Reader::new(bytes_a.clone()),
        )
        .unwrap_or_else(|e| panic!("re-parse failed: {e}"));

        let bytes_b = encoder::encode(&reparsed).unwrap_or_else(|e| panic!("second encode failed: {e}"));
        assert_eq!(bytes_a, bytes_b, "encode stability failed for binary input");
    }

    // =======================================================================
    // Structural edge cases
    // =======================================================================

    #[test]
    fn empty_module() {
        let bytes = assert_wat_round_trip_returning_bytes("(module)");
        // Empty module: magic (4) + version (4) = 8 bytes
        assert_eq!(bytes.len(), 8, "empty module should be 8 bytes");
        assert_eq!(&bytes[0..4], b"\0asm");
        assert_eq!(&bytes[4..8], &1u32.to_le_bytes());
    }

    /// Helper that returns the encoded bytes for further assertions.
    fn assert_wat_round_trip_returning_bytes(wat_source: &str) -> Vec<u8> {
        let module = wat::parse(wat_source).unwrap();
        let bytes_a = encoder::encode(&module).unwrap();
        let reparsed = parser::parse(
            &HashMap::new(),
            "test",
            &mut parser::reader::Reader::new(bytes_a.clone()),
        )
        .unwrap();
        let bytes_b = encoder::encode(&reparsed).unwrap();
        assert_eq!(bytes_a, bytes_b, "encode stability failed");
        bytes_a
    }

    #[test]
    fn module_with_only_start_section() {
        assert_wat_round_trip(
            r#"(module
                (func $entry)
                (start $entry)
            )"#,
        );
    }

    // =======================================================================
    // Type section
    // =======================================================================

    #[test]
    fn type_section_empty_signature() {
        // () -> ()
        assert_wat_round_trip("(module (func))");
    }

    #[test]
    fn type_section_params_and_results() {
        assert_wat_round_trip(
            r#"(module
                (func (param i32 i64) (result f32)
                    f32.const 0)
            )"#,
        );
    }

    #[test]
    fn type_section_multiple_types() {
        assert_wat_round_trip(
            r#"(module
                (type (func (param i32) (result i32)))
                (type (func (param i64) (result i64)))
                (func (type 0) local.get 0)
                (func (type 1) local.get 0)
            )"#,
        );
    }

    // =======================================================================
    // Import section
    // =======================================================================

    #[test]
    fn import_function() {
        assert_wat_round_trip(
            r#"(module
                (import "env" "f" (func (param i32) (result i32)))
            )"#,
        );
    }

    #[test]
    fn import_table() {
        assert_wat_round_trip(
            r#"(module
                (import "env" "tbl" (table 10 funcref))
            )"#,
        );
    }

    #[test]
    fn import_memory() {
        assert_wat_round_trip(r#"(module (import "env" "mem" (memory 1)))"#);
    }

    #[test]
    fn import_memory_with_max() {
        assert_wat_round_trip(r#"(module (import "env" "mem" (memory 1 16)))"#);
    }

    #[test]
    fn import_global() {
        assert_wat_round_trip(r#"(module (import "env" "g" (global (mut i32))))"#);
    }

    #[test]
    fn import_all_kinds() {
        assert_wat_round_trip(
            r#"(module
                (import "env" "f" (func))
                (import "env" "t" (table 0 funcref))
                (import "env" "m" (memory 1))
                (import "env" "g" (global i64))
            )"#,
        );
    }

    // =======================================================================
    // Function + Code section
    // =======================================================================

    #[test]
    fn function_no_locals() {
        assert_wat_round_trip(
            r#"(module
                (func (result i32)
                    i32.const 42)
            )"#,
        );
    }

    #[test]
    fn function_with_locals() {
        assert_wat_round_trip(
            r#"(module
                (func (local i32 i32 i32 i64)
                    nop)
            )"#,
        );
    }

    #[test]
    fn function_with_block_loop_if() {
        // Uses folded syntax for control flow (WAT parser requirement)
        assert_wat_round_trip(
            r#"(module
                (func (param i32) (result i32)
                    (block (result i32)
                        (if (result i32) (local.get 0)
                            (then (i32.const 1))
                            (else
                                (loop (result i32)
                                    (i32.const 0))))))
            )"#,
        );
    }

    #[test]
    fn multiple_functions() {
        assert_wat_round_trip(
            r#"(module
                (func $a (result i32) i32.const 1)
                (func $b (result i32) i32.const 2)
                (func $c (param i32 i32) (result i32)
                    local.get 0
                    local.get 1
                    i32.add)
            )"#,
        );
    }

    // =======================================================================
    // Memory + Table section
    // =======================================================================

    #[test]
    fn memory_min_only() {
        assert_wat_round_trip("(module (memory 1))");
    }

    #[test]
    fn memory_min_and_max() {
        assert_wat_round_trip("(module (memory 1 16))");
    }

    #[test]
    fn table_funcref() {
        assert_wat_round_trip("(module (table 10 funcref))");
    }

    #[test]
    fn table_externref() {
        assert_wat_round_trip("(module (table 0 externref))");
    }

    #[test]
    fn table_with_max() {
        assert_wat_round_trip("(module (table 1 100 funcref))");
    }

    // =======================================================================
    // Global section
    // =======================================================================

    #[test]
    fn global_immutable_i32() {
        assert_wat_round_trip("(module (global i32 (i32.const 42)))");
    }

    #[test]
    fn global_mutable_i64() {
        assert_wat_round_trip("(module (global (mut i64) (i64.const 0)))");
    }

    #[test]
    fn global_f32() {
        assert_wat_round_trip("(module (global f32 (f32.const 3.14)))");
    }

    #[test]
    fn global_f64() {
        assert_wat_round_trip("(module (global f64 (f64.const 2.718)))");
    }

    #[test]
    fn global_mutable_and_immutable() {
        assert_wat_round_trip(
            r#"(module
                (global $a i32 (i32.const 0))
                (global $b (mut i32) (i32.const 100))
            )"#,
        );
    }

    // =======================================================================
    // Export section
    // =======================================================================

    #[test]
    fn export_function() {
        assert_wat_round_trip(
            r#"(module
                (func $f (result i32) i32.const 0)
                (export "f" (func $f))
            )"#,
        );
    }

    #[test]
    fn export_memory() {
        assert_wat_round_trip(
            r#"(module
                (memory 1)
                (export "memory" (memory 0))
            )"#,
        );
    }

    #[test]
    fn export_table() {
        assert_wat_round_trip(
            r#"(module
                (table 0 funcref)
                (export "tbl" (table 0))
            )"#,
        );
    }

    #[test]
    fn export_global() {
        assert_wat_round_trip(
            r#"(module
                (global i32 (i32.const 0))
                (export "g" (global 0))
            )"#,
        );
    }

    #[test]
    fn export_all_kinds() {
        assert_wat_round_trip(
            r#"(module
                (func $f)
                (table 0 funcref)
                (memory 1)
                (global i32 (i32.const 0))
                (export "f" (func $f))
                (export "t" (table 0))
                (export "m" (memory 0))
                (export "g" (global 0))
            )"#,
        );
    }

    // =======================================================================
    // Start section
    // =======================================================================

    #[test]
    fn start_section() {
        assert_wat_round_trip(
            r#"(module
                (func $init nop)
                (start $init)
            )"#,
        );
    }

    // =======================================================================
    // Element section (flags 0-7)
    // =======================================================================

    #[test]
    fn element_flag_0_active_table0_func_indices() {
        assert_wat_round_trip(
            r#"(module
                (table 2 funcref)
                (func $a)
                (func $b)
                (elem (i32.const 0) $a $b)
            )"#,
        );
    }

    #[test]
    fn element_flag_1_passive_func_indices() {
        assert_wat_round_trip(
            r#"(module
                (func $a)
                (elem func $a)
            )"#,
        );
    }

    #[test]
    fn element_flag_2_active_explicit_table() {
        assert_wat_round_trip(
            r#"(module
                (table $t 2 funcref)
                (func $a)
                (elem (table $t) (i32.const 0) func $a)
            )"#,
        );
    }

    #[test]
    fn element_flag_3_declarative() {
        assert_wat_round_trip(
            r#"(module
                (func $a)
                (elem declare func $a)
            )"#,
        );
    }

    #[test]
    fn element_flag_4_active_table0_expr() {
        assert_wat_round_trip(
            r#"(module
                (table 2 funcref)
                (func $a)
                (elem (i32.const 0) (ref.func $a))
            )"#,
        );
    }

    #[test]
    fn element_flag_5_passive_reftype_expr() {
        assert_wat_round_trip(
            r#"(module
                (func $a)
                (elem funcref (ref.func $a))
            )"#,
        );
    }

    #[test]
    fn element_flag_6_active_explicit_table_reftype_expr() {
        assert_wat_round_trip(
            r#"(module
                (table $t 2 funcref)
                (func $a)
                (elem (table $t) (i32.const 0) funcref (ref.func $a))
            )"#,
        );
    }

    #[test]
    fn element_flag_7_declarative_reftype_expr() {
        assert_wat_round_trip(
            r#"(module
                (func $a)
                (elem declare funcref (ref.func $a))
            )"#,
        );
    }

    // =======================================================================
    // Data section
    // =======================================================================

    #[test]
    fn data_active_memory0() {
        assert_wat_round_trip(
            r#"(module
                (memory 1)
                (data (i32.const 0) "hello")
            )"#,
        );
    }

    #[test]
    fn data_passive() {
        assert_wat_round_trip(
            r#"(module
                (memory 1)
                (data "passive data")
            )"#,
        );
    }

    #[test]
    fn data_multiple_segments() {
        assert_wat_round_trip(
            r#"(module
                (memory 1)
                (data (i32.const 0) "first")
                (data (i32.const 100) "second")
            )"#,
        );
    }

    // =======================================================================
    // DataCount section
    // =======================================================================

    #[test]
    fn no_data_count_without_bulk_memory_ops() {
        // DataCount section is only required when memory.init or data.drop are used.
        // Plain data segments without those instructions should not emit DataCount.
        let bytes = assert_wat_round_trip_returning_bytes(
            r#"(module
                (memory 1)
                (data (i32.const 0) "a")
                (data (i32.const 10) "b")
                (data (i32.const 20) "c")
            )"#,
        );
        assert!(
            find_section(&bytes, 12).is_none(),
            "data count section should be absent without memory.init/data.drop"
        );
    }

    #[test]
    fn data_count_present_with_data_drop() {
        // WAT parser doesn't support bulk memory ops, so construct binary directly.
        // Module: (type () -> ()) (func (data.drop 0)) (memory 1) (data "a")
        let wasm: &[u8] = &[
            0x00, 0x61, 0x73, 0x6D, 0x01, 0x00, 0x00, 0x00, // header
            0x01, 0x04, 0x01, 0x60, 0x00, 0x00, // type section: () -> ()
            0x03, 0x02, 0x01, 0x00, // function section: 1 func, type 0
            0x05, 0x03, 0x01, 0x00, 0x01, // memory section: 1 mem, min=1
            0x0C, 0x01, 0x01, // data count section: count=1
            0x0A, 0x07, 0x01, 0x05, 0x00, 0xFC, 0x09, 0x00, 0x0B, // code: data.drop 0
            0x0B, 0x07, 0x01, 0x00, 0x41, 0x00, 0x0B, 0x01, 0x61, // data: active "a"
        ];
        let module = parser::parse(
            &HashMap::new(),
            "data-drop-test",
            &mut parser::reader::Reader::new(wasm.to_vec()),
        )
        .unwrap();
        let encoded = encoder::encode(&module).unwrap();
        assert!(
            find_section(&encoded, 12).is_some(),
            "data count section should be present when data.drop is used"
        );
    }

    #[test]
    fn no_data_count_when_no_data() {
        let bytes = assert_wat_round_trip_returning_bytes("(module (func))");
        assert!(
            find_section(&bytes, 12).is_none(),
            "data count section should be absent when no data segments"
        );
    }

    /// Scans encoded bytes for a section with the given ID.
    /// Returns the offset of the section ID byte, or None.
    fn find_section(bytes: &[u8], section_id: u8) -> Option<usize> {
        let mut pos = 8; // skip magic + version
        while pos < bytes.len() {
            let id = bytes[pos];
            pos += 1;
            // Read LEB128 section length
            let (len, consumed) = read_leb128_u32(&bytes[pos..]);
            pos += consumed;
            if id == section_id {
                return Some(pos - consumed - 1);
            }
            pos += len as usize;
        }
        None
    }

    /// Minimal LEB128 u32 reader for test assertions.
    fn read_leb128_u32(bytes: &[u8]) -> (u32, usize) {
        let mut result: u32 = 0;
        let mut shift = 0;
        for (i, &byte) in bytes.iter().enumerate() {
            result |= ((byte & 0x7F) as u32) << shift;
            if byte & 0x80 == 0 {
                return (result, i + 1);
            }
            shift += 7;
        }
        (result, bytes.len())
    }

    // =======================================================================
    // Custom sections
    // =======================================================================

    #[test]
    fn custom_section_round_trip() {
        // Custom sections parsed from binary round-trip correctly.
        // Build a binary with a custom section manually.
        let mut wasm = Vec::new();
        wasm.extend_from_slice(b"\0asm");
        wasm.extend_from_slice(&1u32.to_le_bytes());
        // Custom section: id=0, name="test", data=[1, 2, 3]
        let name = b"test";
        let data = [1u8, 2, 3];
        let section_len = 1 + name.len() + data.len(); // name_len_byte + name + data
        wasm.push(0x00); // section id
        wasm.push(section_len as u8); // section byte length
        wasm.push(name.len() as u8); // name length
        wasm.extend_from_slice(name);
        wasm.extend_from_slice(&data);

        assert_binary_round_trip(&wasm);
    }

    // =======================================================================
    // WAT round-trip: increasing complexity
    // =======================================================================

    #[test]
    fn wat_round_trip_minimal_function() {
        assert_wat_round_trip("(module (func))");
    }

    #[test]
    fn wat_round_trip_exported_add() {
        assert_wat_round_trip(
            r#"(module
                (func (export "add") (param i32 i32) (result i32)
                    local.get 0
                    local.get 1
                    i32.add)
            )"#,
        );
    }

    #[test]
    fn wat_round_trip_memory_and_data() {
        assert_wat_round_trip(
            r#"(module
                (memory 1)
                (data (i32.const 0) "Hello, World!\n")
                (func $main (result i32)
                    i32.const 0)
                (export "memory" (memory 0))
                (export "main" (func $main))
            )"#,
        );
    }

    #[test]
    fn wat_round_trip_globals_and_imports() {
        assert_wat_round_trip(
            r#"(module
                (import "env" "log" (func $log (param i32)))
                (global $counter (mut i32) (i32.const 0))
                (func (export "inc")
                    global.get $counter
                    i32.const 1
                    i32.add
                    global.set $counter
                    global.get $counter
                    call $log)
            )"#,
        );
    }

    #[test]
    fn wat_round_trip_table_and_call_indirect() {
        assert_wat_round_trip(
            r#"(module
                (type $sig (func (result i32)))
                (table 3 funcref)
                (func $a (type $sig) i32.const 1)
                (func $b (type $sig) i32.const 2)
                (func $call (param i32) (result i32)
                    local.get 0
                    call_indirect (type $sig))
                (elem (i32.const 0) $a $b)
                (export "call" (func $call))
            )"#,
        );
    }

    #[test]
    fn wat_round_trip_complex_module() {
        assert_wat_round_trip(
            r#"(module
                (type $void (func))
                (type $i32_i32 (func (param i32 i32) (result i32)))
                (import "env" "print" (func $print (type $void)))
                (table 4 funcref)
                (memory 1 4)
                (global $x (mut i32) (i32.const 0))
                (global $y i64 (i64.const 100))
                (func $add (type $i32_i32)
                    local.get 0
                    local.get 1
                    i32.add)
                (func $entry (type $void)
                    call $print)
                (export "add" (func $add))
                (export "memory" (memory 0))
                (export "table" (table 0))
                (start $entry)
                (elem (i32.const 0) $add $entry)
                (data (i32.const 0) "hello world")
            )"#,
        );
    }

    // =======================================================================
    // Binary round-trips with spec test fixtures
    // =======================================================================

    // Spec test JSON structure for loading binaries.
    #[derive(Deserialize)]
    struct SpecTestData {
        bin: HashMap<String, Base64Bytes>,
    }

    #[derive(Debug)]
    struct Base64Bytes(Vec<u8>);

    impl<'de> Deserialize<'de> for Base64Bytes {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            let s: String = String::deserialize(deserializer)?;
            let decoded = general_purpose::STANDARD.decode(s).map_err(de::Error::custom)?;
            Ok(Base64Bytes(decoded))
        }
    }

    /// Loads all valid (parseable) binary modules from a spec test JSON,
    /// encodes each, and compares against the original bytes.
    ///
    /// Byte-exact match is expected for all spec fixture modules except two
    /// from custom.json that intentionally interleave custom sections between
    /// standard sections (the spec permits this per §5.5.2). Our encoder
    /// always emits custom sections at the end, so the bytes differ in
    /// section ordering. For these two modules only, encode stability is
    /// verified instead.
    ///
    /// - custom.1.wasm: exhaustive placement test — two custom sections
    ///   sandwiching every standard section
    /// - custom.2.wasm: functional module (addTwo) with a custom section
    ///   between Type and Function
    const INTERLEAVED_CUSTOM_SECTION_MODULES: &[&str] = &["custom.1.wasm", "custom.2.wasm"];

    fn round_trip_spec_fixture(fixture: &str) {
        let path = format!("tests/spec/{fixture}.json");
        let json_str = fs::read_to_string(&path).unwrap_or_else(|e| panic!("failed to read {path}: {e}"));
        let data: SpecTestData =
            serde_json::from_str(&json_str).unwrap_or_else(|e| panic!("failed to parse {path}: {e}"));

        let mut tested = 0;
        for (name, original) in &data.bin {
            let parse_result = parser::parse(
                &HashMap::new(),
                name,
                &mut parser::reader::Reader::new(original.0.clone()),
            );
            if let Ok(module) = parse_result {
                let encoded = encoder::encode(&module).unwrap_or_else(|e| panic!("encode failed for {name}: {e}"));

                if encoded == original.0 {
                    tested += 1;
                    continue;
                }

                if INTERLEAVED_CUSTOM_SECTION_MODULES.contains(&name.as_str()) {
                    // These modules intentionally place custom sections between
                    // standard sections. Our encoder emits custom sections at the
                    // end, so byte layout differs. Verify encode stability instead.
                    let reparsed =
                        parser::parse(&HashMap::new(), name, &mut parser::reader::Reader::new(encoded.clone()))
                            .unwrap_or_else(|e| panic!("re-parse failed for {name}: {e}"));
                    let re_encoded =
                        encoder::encode(&reparsed).unwrap_or_else(|e| panic!("re-encode failed for {name}: {e}"));
                    assert_eq!(encoded, re_encoded, "encode stability failed for {name} in {fixture}");
                } else {
                    let first_diff = original
                        .0
                        .iter()
                        .zip(encoded.iter())
                        .position(|(a, b)| a != b)
                        .unwrap_or(original.0.len().min(encoded.len()));
                    panic!(
                        "byte-exact mismatch for {name} in {fixture}: \
                         original={} bytes, encoded={} bytes, \
                         first diff at byte {first_diff} \
                         (original=0x{:02x}, encoded=0x{:02x})",
                        original.0.len(),
                        encoded.len(),
                        original.0.get(first_diff).copied().unwrap_or(0),
                        encoded.get(first_diff).copied().unwrap_or(0),
                    );
                }
                tested += 1;
            }
        }
        assert!(tested > 0, "expected at least one valid module in {fixture}");
    }

    #[test]
    fn spec_exports() {
        round_trip_spec_fixture("exports");
    }

    #[test]
    fn spec_func() {
        round_trip_spec_fixture("func");
    }

    #[test]
    fn spec_global() {
        round_trip_spec_fixture("global");
    }

    #[test]
    fn spec_memory() {
        round_trip_spec_fixture("memory");
    }

    #[test]
    fn spec_table() {
        round_trip_spec_fixture("table");
    }

    #[test]
    fn spec_data() {
        round_trip_spec_fixture("data");
    }

    #[test]
    fn spec_elem() {
        round_trip_spec_fixture("elem");
    }

    #[test]
    fn spec_start() {
        round_trip_spec_fixture("start");
    }

    #[test]
    fn spec_imports() {
        round_trip_spec_fixture("imports");
    }

    #[test]
    fn spec_type() {
        round_trip_spec_fixture("type");
    }

    #[test]
    fn spec_custom() {
        round_trip_spec_fixture("custom");
    }

    #[test]
    fn spec_call() {
        round_trip_spec_fixture("call");
    }

    #[test]
    fn spec_call_indirect() {
        round_trip_spec_fixture("call_indirect");
    }

    #[test]
    fn spec_block() {
        round_trip_spec_fixture("block");
    }

    #[test]
    fn spec_loop() {
        round_trip_spec_fixture("loop");
    }

    #[test]
    fn spec_if() {
        round_trip_spec_fixture("if");
    }

    #[test]
    fn spec_br() {
        round_trip_spec_fixture("br");
    }

    #[test]
    fn spec_br_table() {
        round_trip_spec_fixture("br_table");
    }

    #[test]
    fn spec_select() {
        round_trip_spec_fixture("select");
    }

    #[test]
    fn spec_nop() {
        round_trip_spec_fixture("nop");
    }

    #[test]
    fn spec_fac() {
        round_trip_spec_fixture("fac");
    }

    #[test]
    fn spec_address() {
        round_trip_spec_fixture("address");
    }

    #[test]
    fn spec_bulk() {
        round_trip_spec_fixture("bulk");
    }

    #[test]
    fn round_trip_simd_fixtures() {
        let mut failures = Vec::new();
        let mut count = 0;
        for entry in std::fs::read_dir("tests/spec").unwrap() {
            let path = entry.unwrap().path();
            let name = path.file_stem().unwrap().to_str().unwrap().to_string();
            if name.starts_with("simd_") {
                count += 1;
                if let Err(e) = std::panic::catch_unwind(|| round_trip_spec_fixture(&name)) {
                    let msg = e
                        .downcast_ref::<String>()
                        .map(|s| s.as_str())
                        .or_else(|| e.downcast_ref::<&str>().copied())
                        .unwrap_or("unknown panic");
                    failures.push(format!("{name}: {msg}"));
                }
            }
        }
        assert!(count > 0, "no simd_* fixtures found");
        if !failures.is_empty() {
            panic!(
                "{}/{} SIMD fixtures failed:\n  {}",
                failures.len(),
                count,
                failures.join("\n  ")
            );
        }
    }

    // =======================================================================
    // Negative tests
    // =======================================================================

    #[test]
    fn invalid_element_flags() {
        // Construct a Module with element flags outside 0-7.
        use kasm::parser::module::Element;
        use kasm::parser::module::{ElementMode, ElementSection, Module, RefType, SectionPosition};

        let mut module = Module::new("test");
        module.magic = 0x6d736100;
        module.version = 1;
        module.elements = ElementSection {
            elements: vec![Element {
                flags: 8, // invalid
                ref_type: RefType::FuncRef,
                mode: ElementMode::Passive,
                init: vec![],
            }],
            position: SectionPosition::new(0, 0),
        };

        let result = encoder::encode(&module);
        assert!(result.is_err(), "expected error for element flags=8");
        let err = result.unwrap_err();
        assert!(
            format!("{err}").contains("invalid element segment flags: 8"),
            "error message should mention flags: {err}"
        );
    }

    #[test]
    fn empty_code_nonempty_functions() {
        // Module with function declarations but no code bodies.
        // The encoder should produce valid (though semantically invalid) binary
        // without panicking.
        use kasm::parser::module::{
            CodeSection, Function, FunctionSection, FunctionType, Module, SectionPosition, TypeSection,
        };

        let mut module = Module::new("test");
        module.magic = 0x6d736100;
        module.version = 1;
        module.types = TypeSection {
            types: vec![FunctionType {
                parameters: vec![],
                return_types: vec![],
            }],
            position: SectionPosition::new(1, 1),
        };
        module.functions = FunctionSection {
            functions: vec![Function { ftype_index: 0 }],
            position: SectionPosition::new(1, 1),
        };
        module.code = CodeSection {
            code: vec![],
            position: SectionPosition::new(0, 0),
        };

        // Should not panic
        let result = encoder::encode(&module);
        assert!(
            result.is_ok(),
            "encoder should not panic on mismatched function/code counts"
        );
    }

    // =======================================================================
    // Exact byte assertions for specific sections
    // =======================================================================

    #[test]
    fn type_section_exact_bytes() {
        let bytes = assert_wat_round_trip_returning_bytes(
            r#"(module
                (type (func (param i32 i64) (result f32)))
            )"#,
        );
        // After magic (4) + version (4) = 8 bytes, type section begins.
        // Section ID: 0x01
        // Section length: LEB128
        // Count: 1
        // functype marker: 0x60
        // params: count=2, i32=0x7F, i64=0x7E
        // results: count=1, f32=0x7D
        assert_eq!(bytes[8], 0x01, "section id should be 1 (type)");
        // section length = 7 (1 count + 1 functype + 1 param_count + 2 params + 1 result_count + 1 result)
        assert_eq!(bytes[9], 0x07, "section length");
        assert_eq!(bytes[10], 0x01, "type count = 1");
        assert_eq!(bytes[11], 0x60, "functype marker");
        assert_eq!(bytes[12], 0x02, "param count = 2");
        assert_eq!(bytes[13], 0x7F, "param 0 = i32");
        assert_eq!(bytes[14], 0x7E, "param 1 = i64");
        assert_eq!(bytes[15], 0x01, "result count = 1");
        assert_eq!(bytes[16], 0x7D, "result 0 = f32");
    }

    #[test]
    fn memory_section_exact_bytes() {
        let bytes = assert_wat_round_trip_returning_bytes("(module (memory 1 16))");
        // After type section (empty → absent), memory section starts at offset 8.
        assert_eq!(bytes[8], 0x05, "section id should be 5 (memory)");
        // length=4: 1 count + 1 has_max + 1 min + 1 max
        assert_eq!(bytes[9], 0x04, "section length");
        assert_eq!(bytes[10], 0x01, "memory count = 1");
        assert_eq!(bytes[11], 0x01, "has_max = true");
        assert_eq!(bytes[12], 0x01, "min = 1");
        assert_eq!(bytes[13], 0x10, "max = 16");
    }

    #[test]
    fn export_section_exact_bytes() {
        let bytes = assert_wat_round_trip_returning_bytes(
            r#"(module
                (func (export "f") (result i32)
                    i32.const 0)
            )"#,
        );
        // Find export section (id 7)
        let offset = find_section(&bytes, 7).expect("export section should exist");
        assert_eq!(bytes[offset], 0x07, "section id");
        let (sec_len, len_bytes) = read_leb128_u32(&bytes[offset + 1..]);
        let contents_start = offset + 1 + len_bytes;
        let contents = &bytes[contents_start..contents_start + sec_len as usize];
        // count=1, name_len=1, name='f', export_kind=0 (func), index=0
        assert_eq!(contents[0], 0x01, "export count = 1");
        assert_eq!(contents[1], 0x01, "name length = 1");
        assert_eq!(contents[2], b'f', "name = 'f'");
        assert_eq!(contents[3], 0x00, "export kind = function");
        assert_eq!(contents[4], 0x00, "function index = 0");
    }

    #[test]
    fn global_section_exact_bytes() {
        let bytes = assert_wat_round_trip_returning_bytes("(module (global (mut i32) (i32.const 42)))");
        let offset = find_section(&bytes, 6).expect("global section should exist");
        assert_eq!(bytes[offset], 0x06, "section id");
        let (_, len_bytes) = read_leb128_u32(&bytes[offset + 1..]);
        let contents_start = offset + 1 + len_bytes;
        assert_eq!(bytes[contents_start], 0x01, "global count = 1");
        assert_eq!(bytes[contents_start + 1], 0x7F, "value type = i32");
        assert_eq!(bytes[contents_start + 2], 0x01, "mutable = true");
        // Init expression: i32.const 42 (0x41 0x2A) end (0x0B)
        assert_eq!(bytes[contents_start + 3], 0x41, "i32.const opcode");
        assert_eq!(bytes[contents_start + 4], 42, "i32.const value = 42");
        assert_eq!(bytes[contents_start + 5], 0x0B, "end marker");
    }
}
