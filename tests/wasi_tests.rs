//! Integration tests for WASI functionality

mod common;

use common::CapturedWriter;
use kasm::parser;
use kasm::parser::reader::Reader;
use kasm::runtime::store::Store;
use kasm::runtime::wasi::{MEMORY_EXPORT, WasiContext, add_assemblyscript_imports, create_wasi_imports};
use std::collections::HashMap;
use std::io::Cursor;
use std::sync::{Arc, Mutex};

/// Helper to compile WAT to WASM
fn wat_to_wasm(wat: &str) -> Vec<u8> {
    wat::parse_str(wat).expect("Failed to parse WAT")
}

#[test]
fn test_hello_wasi() {
    let wat = r#"
    ;; Smoke test for fd_write - prints "Hello from WASI!" to stdout
    (module
      (import "wasi_snapshot_preview1" "fd_write"
        (func $fd_write (param i32 i32 i32 i32) (result i32)))
      (memory (export "memory") 1)

      ;; "Hello from WASI!\n" at offset 8 (17 bytes)
      (data (i32.const 8) "Hello from WASI!\n")

      ;; iovec at offset 0: {ptr: 8, len: 17}
      (data (i32.const 0) "\08\00\00\00\11\00\00\00")

      (func (export "_start")
        (drop (call $fd_write
          (i32.const 1)   ;; fd = stdout
          (i32.const 0)   ;; iovs ptr
          (i32.const 1)   ;; iovs len
          (i32.const 100) ;; nwritten ptr
        ))
      )
    )
    "#;
    let wasm = wat_to_wasm(wat);

    // Parse the module
    let module = parser::parse(&HashMap::new(), "test", &mut Reader::new(wasm)).expect("Failed to parse module");

    // Create captured stdout
    let stdout_buffer = Arc::new(Mutex::new(Vec::<u8>::new()));
    let stdout = CapturedWriter(stdout_buffer.clone());

    // Create WASI context
    let ctx = Arc::new(
        WasiContext::builder()
            .args(["hello.wasm"])
            .stdout(Box::new(stdout))
            .build(),
    );

    // Create store and imports
    let mut store = Store::new();
    let imports = create_wasi_imports(&mut store, ctx.clone());

    // Create instance
    let instance_id = store
        .create_instance(&module, Some(&imports))
        .expect("Failed to create instance");

    // Bind memory to WASI context
    let memory_addr = store
        .get_instance(instance_id)
        .unwrap()
        .get_memory_addr("memory")
        .expect("Failed to get memory");
    ctx.bind_memory(store.get_memory(memory_addr).unwrap().clone());

    // Execute _start
    let result = store.invoke_export(instance_id, "_start", vec![], None);
    assert!(result.is_ok(), "Execution failed: {:?}", result);

    // Check output
    let output = stdout_buffer.lock().unwrap();
    assert_eq!(&*output, b"Hello from WASI!\n");
}

#[test]
fn test_fd_read_stdin() {
    let wat = r#"
    (module
      (import "wasi_snapshot_preview1" "fd_read"
        (func $fd_read (param i32 i32 i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "fd_write"
        (func $fd_write (param i32 i32 i32 i32) (result i32)))
      (memory (export "memory") 1)

      ;; Read buffer at offset 100 (64 bytes)
      ;; iovec at offset 0: {ptr: 100, len: 64}
      (data (i32.const 0) "\64\00\00\00\40\00\00\00")

      (func (export "_start")
        ;; Read from stdin
        (drop (call $fd_read
          (i32.const 0)   ;; fd = stdin
          (i32.const 0)   ;; iovs ptr
          (i32.const 1)   ;; iovs len
          (i32.const 200) ;; nread ptr
        ))

        ;; Update iovec to point to read data with actual length
        (i32.store (i32.const 4) (i32.load (i32.const 200)))

        ;; Write to stdout
        (drop (call $fd_write
          (i32.const 1)   ;; fd = stdout
          (i32.const 0)   ;; iovs ptr
          (i32.const 1)   ;; iovs len
          (i32.const 204) ;; nwritten ptr
        ))
      )
    )
    "#;

    let wasm = wat_to_wasm(wat);
    let module = parser::parse(&HashMap::new(), "test", &mut Reader::new(wasm)).expect("Failed to parse module");

    // Create captured stdout and mock stdin
    let stdout_buffer = Arc::new(Mutex::new(Vec::<u8>::new()));
    let stdout = CapturedWriter(stdout_buffer.clone());
    let stdin = Cursor::new(b"test input\n".to_vec());

    let ctx = Arc::new(
        WasiContext::builder()
            .stdin(Box::new(stdin))
            .stdout(Box::new(stdout))
            .build(),
    );

    let mut store = Store::new();
    let imports = create_wasi_imports(&mut store, ctx.clone());
    let instance_id = store
        .create_instance(&module, Some(&imports))
        .expect("Failed to create instance");

    let memory_addr = store
        .get_instance(instance_id)
        .unwrap()
        .get_memory_addr("memory")
        .expect("Failed to get memory");
    ctx.bind_memory(store.get_memory(memory_addr).unwrap().clone());

    let result = store.invoke_export(instance_id, "_start", vec![], None);
    assert!(result.is_ok(), "Execution failed: {:?}", result);

    // Check that input was echoed to output
    let output = stdout_buffer.lock().unwrap();
    assert_eq!(&*output, b"test input\n");
}

#[test]
fn test_args() {
    let wat = r#"
    (module
      (import "wasi_snapshot_preview1" "args_sizes_get"
        (func $args_sizes_get (param i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "args_get"
        (func $args_get (param i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "fd_write"
        (func $fd_write (param i32 i32 i32 i32) (result i32)))
      (memory (export "memory") 1)

      (global $argc (mut i32) (i32.const 0))
      (global $argv_buf_size (mut i32) (i32.const 0))

      (func (export "_start")
        ;; Get arg sizes (store at 0, 4)
        (drop (call $args_sizes_get (i32.const 0) (i32.const 4)))
        (global.set $argc (i32.load (i32.const 0)))
        (global.set $argv_buf_size (i32.load (i32.const 4)))

        ;; Get args: argv at 100, buf at 200
        (drop (call $args_get (i32.const 100) (i32.const 200)))

        ;; Print first arg (program name)
        ;; Create iovec at 300: {ptr from argv[0], len till null}
        (i32.store (i32.const 300) (i32.load (i32.const 100))) ;; ptr
        (i32.store (i32.const 304) (i32.const 4)) ;; len = "prog" = 4 bytes

        (drop (call $fd_write
          (i32.const 1)
          (i32.const 300)
          (i32.const 1)
          (i32.const 400)
        ))
      )
    )
    "#;

    let wasm = wat_to_wasm(wat);
    let module = parser::parse(&HashMap::new(), "test", &mut Reader::new(wasm)).expect("Failed to parse module");

    let stdout_buffer = Arc::new(Mutex::new(Vec::<u8>::new()));
    let stdout = CapturedWriter(stdout_buffer.clone());

    let ctx = Arc::new(
        WasiContext::builder()
            .args(["prog", "arg1", "arg2"])
            .stdout(Box::new(stdout))
            .build(),
    );

    let mut store = Store::new();
    let imports = create_wasi_imports(&mut store, ctx.clone());
    let instance_id = store
        .create_instance(&module, Some(&imports))
        .expect("Failed to create instance");

    let memory_addr = store
        .get_instance(instance_id)
        .unwrap()
        .get_memory_addr("memory")
        .expect("Failed to get memory");
    ctx.bind_memory(store.get_memory(memory_addr).unwrap().clone());

    let result = store.invoke_export(instance_id, "_start", vec![], None);
    assert!(result.is_ok(), "Execution failed: {:?}", result);

    // Check that "prog" was printed
    let output = stdout_buffer.lock().unwrap();
    assert_eq!(&*output, b"prog");
}

#[test]
fn test_assemblyscript_hello() {
    // Test the pre-built AssemblyScript hello world example
    let wasm = std::fs::read("examples/assemblyscript/build/release.wasm")
        .expect("Failed to read AssemblyScript wasm - run `npm run asbuild` in examples/assemblyscript first");

    let module = parser::parse(&HashMap::new(), "test", &mut Reader::new(wasm)).expect("Failed to parse module");

    let stdout_buffer = Arc::new(Mutex::new(Vec::<u8>::new()));
    let stdout = CapturedWriter(stdout_buffer.clone());

    let ctx = Arc::new(
        WasiContext::builder()
            .args(["hello.wasm"])
            .stdout(Box::new(stdout))
            .build(),
    );

    let mut store = Store::new();
    let mut imports = create_wasi_imports(&mut store, ctx.clone());
    // AssemblyScript requires env.abort
    add_assemblyscript_imports(&mut store, &mut imports, ctx.clone());

    let instance_id = store
        .create_instance(&module, Some(&imports))
        .expect("Failed to create instance");

    let memory_addr = store
        .get_instance(instance_id)
        .unwrap()
        .get_memory_addr(MEMORY_EXPORT)
        .expect("Failed to get memory");
    ctx.bind_memory(store.get_memory(memory_addr).unwrap().clone());

    let result = store.invoke_export(instance_id, "_start", vec![], None);
    assert!(result.is_ok(), "Execution failed: {:?}", result);

    let output = stdout_buffer.lock().unwrap();
    let output_str = String::from_utf8_lossy(&output);
    assert!(
        output_str.contains("Hello from AssemblyScript"),
        "Expected 'Hello from AssemblyScript' in output, got: {}",
        output_str
    );
}

#[test]
fn test_proc_exit() {
    let wat = r#"
    (module
      (import "wasi_snapshot_preview1" "proc_exit"
        (func $proc_exit (param i32)))
      (memory (export "memory") 1)

      (func (export "_start")
        (call $proc_exit (i32.const 42))
      )
    )
    "#;

    let wasm = wat_to_wasm(wat);
    let module = parser::parse(&HashMap::new(), "test", &mut Reader::new(wasm)).expect("Failed to parse module");

    let ctx = Arc::new(WasiContext::builder().build());

    let mut store = Store::new();
    let imports = create_wasi_imports(&mut store, ctx.clone());
    let instance_id = store
        .create_instance(&module, Some(&imports))
        .expect("Failed to create instance");

    let memory_addr = store
        .get_instance(instance_id)
        .unwrap()
        .get_memory_addr("memory")
        .expect("Failed to get memory");
    ctx.bind_memory(store.get_memory(memory_addr).unwrap().clone());

    // proc_exit should trap
    let result = store.invoke_export(instance_id, "_start", vec![], None);
    assert!(result.is_err());

    // Exit code should be 42
    assert_eq!(ctx.exit_code(), Some(42));
}

#[test]
fn test_multiple_iovecs() {
    // Test scatter/gather I/O with multiple iovec entries
    let wat = r#"
    (module
      (import "wasi_snapshot_preview1" "fd_write"
        (func $fd_write (param i32 i32 i32 i32) (result i32)))
      (memory (export "memory") 1)

      ;; Three strings at different offsets
      (data (i32.const 100) "Hello")
      (data (i32.const 110) ", ")
      (data (i32.const 120) "World!")

      ;; Three iovecs starting at offset 0
      ;; iovec[0]: {ptr: 100, len: 5} - "Hello"
      ;; iovec[1]: {ptr: 110, len: 2} - ", "
      ;; iovec[2]: {ptr: 120, len: 6} - "World!"
      (data (i32.const 0) "\64\00\00\00\05\00\00\00")   ;; {100, 5}
      (data (i32.const 8) "\6e\00\00\00\02\00\00\00")   ;; {110, 2}
      (data (i32.const 16) "\78\00\00\00\06\00\00\00")  ;; {120, 6}

      (func (export "_start")
        (drop (call $fd_write
          (i32.const 1)   ;; fd = stdout
          (i32.const 0)   ;; iovs ptr
          (i32.const 3)   ;; iovs len = 3 iovecs
          (i32.const 200) ;; nwritten ptr
        ))
      )
    )
    "#;

    let wasm = wat_to_wasm(wat);
    let module = parser::parse(&HashMap::new(), "test", &mut Reader::new(wasm)).expect("Failed to parse module");

    let stdout_buffer = Arc::new(Mutex::new(Vec::<u8>::new()));
    let stdout = CapturedWriter(stdout_buffer.clone());

    let ctx = Arc::new(WasiContext::builder().stdout(Box::new(stdout)).build());

    let mut store = Store::new();
    let imports = create_wasi_imports(&mut store, ctx.clone());
    let instance_id = store
        .create_instance(&module, Some(&imports))
        .expect("Failed to create instance");

    let memory_addr = store
        .get_instance(instance_id)
        .unwrap()
        .get_memory_addr(MEMORY_EXPORT)
        .expect("Failed to get memory");
    ctx.bind_memory(store.get_memory(memory_addr).unwrap().clone());

    let result = store.invoke_export(instance_id, "_start", vec![], None);
    assert!(result.is_ok(), "Execution failed: {:?}", result);

    let output = stdout_buffer.lock().unwrap();
    assert_eq!(&*output, b"Hello, World!");
}

#[test]
fn test_fd_read_eof() {
    // Test that reading from empty stdin returns 0 bytes (EOF)
    let wat = r#"
    (module
      (import "wasi_snapshot_preview1" "fd_read"
        (func $fd_read (param i32 i32 i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "fd_write"
        (func $fd_write (param i32 i32 i32 i32) (result i32)))
      (memory (export "memory") 1)

      ;; iovec at offset 0: {ptr: 100, len: 64}
      (data (i32.const 0) "\64\00\00\00\40\00\00\00")

      (func (export "_start") (result i32)
        ;; Read from stdin (which is empty)
        (drop (call $fd_read
          (i32.const 0)   ;; fd = stdin
          (i32.const 0)   ;; iovs ptr
          (i32.const 1)   ;; iovs len
          (i32.const 200) ;; nread ptr
        ))
        ;; Return bytes read (should be 0 for EOF)
        (i32.load (i32.const 200))
      )
    )
    "#;

    let wasm = wat_to_wasm(wat);
    let module = parser::parse(&HashMap::new(), "test", &mut Reader::new(wasm)).expect("Failed to parse module");

    // Empty stdin
    let stdin = Cursor::new(Vec::<u8>::new());

    let ctx = Arc::new(WasiContext::builder().stdin(Box::new(stdin)).build());

    let mut store = Store::new();
    let imports = create_wasi_imports(&mut store, ctx.clone());
    let instance_id = store
        .create_instance(&module, Some(&imports))
        .expect("Failed to create instance");

    let memory_addr = store
        .get_instance(instance_id)
        .unwrap()
        .get_memory_addr(MEMORY_EXPORT)
        .expect("Failed to get memory");
    ctx.bind_memory(store.get_memory(memory_addr).unwrap().clone());

    let result = store.invoke_export(instance_id, "_start", vec![], None);
    assert!(result.is_ok(), "Execution failed: {:?}", result);

    // Function returns nread, which should be 0 for EOF
    let values = result.unwrap();
    assert_eq!(values.len(), 1);
    if let kasm::runtime::Value::I32(nread) = values[0] {
        assert_eq!(nread, 0, "Expected 0 bytes read for EOF");
    } else {
        panic!("Expected i32 return value");
    }
}

#[test]
fn test_environ() {
    // Test environment variable access - get sizes, fetch values, print them
    let wat = r#"
    (module
      (import "wasi_snapshot_preview1" "environ_sizes_get"
        (func $environ_sizes_get (param i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "environ_get"
        (func $environ_get (param i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "fd_write"
        (func $fd_write (param i32 i32 i32 i32) (result i32)))
      (memory (export "memory") 1)

      ;; Memory layout:
      ;; 0-3: environc
      ;; 4-7: environ_buf_size
      ;; 100-119: environ pointers (up to 5 env vars)
      ;; 200-399: environ string buffer
      ;; 400-407: iovec for writing
      ;; 408-411: nwritten

      (func (export "_start") (result i32)
        (local $i i32)
        (local $ptr i32)
        (local $len i32)

        ;; Get environ sizes
        (drop (call $environ_sizes_get (i32.const 0) (i32.const 4)))

        ;; Get environ data: pointers at 100, strings at 200
        (drop (call $environ_get (i32.const 100) (i32.const 200)))

        ;; Print each environment variable
        (local.set $i (i32.const 0))
        (block $done
          (loop $loop
            ;; Check if we've printed all env vars
            (br_if $done (i32.ge_u (local.get $i) (i32.load (i32.const 0))))

            ;; Get pointer to this env var string
            (local.set $ptr (i32.load (i32.add (i32.const 100) (i32.mul (local.get $i) (i32.const 4)))))

            ;; Calculate string length (scan for null terminator)
            (local.set $len (i32.const 0))
            (block $len_done
              (loop $len_loop
                (br_if $len_done (i32.eqz (i32.load8_u (i32.add (local.get $ptr) (local.get $len)))))
                (local.set $len (i32.add (local.get $len) (i32.const 1)))
                (br $len_loop)
              )
            )

            ;; Set up iovec at 400: {ptr, len}
            (i32.store (i32.const 400) (local.get $ptr))
            (i32.store (i32.const 404) (local.get $len))

            ;; Write env var to stdout
            (drop (call $fd_write (i32.const 1) (i32.const 400) (i32.const 1) (i32.const 408)))

            ;; Write newline
            (i32.store8 (i32.const 450) (i32.const 10))
            (i32.store (i32.const 400) (i32.const 450))
            (i32.store (i32.const 404) (i32.const 1))
            (drop (call $fd_write (i32.const 1) (i32.const 400) (i32.const 1) (i32.const 408)))

            ;; Next env var
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $loop)
          )
        )

        ;; Return environc
        (i32.load (i32.const 0))
      )
    )
    "#;

    let wasm = wat_to_wasm(wat);
    let module = parser::parse(&HashMap::new(), "test", &mut Reader::new(wasm)).expect("Failed to parse module");

    let stdout_buffer = Arc::new(Mutex::new(Vec::<u8>::new()));
    let stdout = CapturedWriter(stdout_buffer.clone());

    let ctx = Arc::new(
        WasiContext::builder()
            .env(["FOO=bar", "PATH=/usr/bin"])
            .stdout(Box::new(stdout))
            .build(),
    );

    let mut store = Store::new();
    let imports = create_wasi_imports(&mut store, ctx.clone());
    let instance_id = store
        .create_instance(&module, Some(&imports))
        .expect("Failed to create instance");

    let memory_addr = store
        .get_instance(instance_id)
        .unwrap()
        .get_memory_addr(MEMORY_EXPORT)
        .expect("Failed to get memory");
    ctx.bind_memory(store.get_memory(memory_addr).unwrap().clone());

    let result = store.invoke_export(instance_id, "_start", vec![], None);
    assert!(result.is_ok(), "Execution failed: {:?}", result);

    // Check environc return value
    let values = result.unwrap();
    assert_eq!(values.len(), 1);
    if let kasm::runtime::Value::I32(environc) = values[0] {
        assert_eq!(environc, 2, "Expected 2 environment variables");
    } else {
        panic!("Expected i32 return value");
    }

    // Check printed output
    let output = stdout_buffer.lock().unwrap();
    let output_str = String::from_utf8_lossy(&output);
    assert!(
        output_str.contains("FOO=bar"),
        "Expected 'FOO=bar' in output, got: {}",
        output_str
    );
    assert!(
        output_str.contains("PATH=/usr/bin"),
        "Expected 'PATH=/usr/bin' in output, got: {}",
        output_str
    );
}

#[test]
fn test_fd_prestat_returns_badf() {
    // Test that fd_prestat_get returns EBADF (no preopened directories)
    let wat = r#"
    (module
      (import "wasi_snapshot_preview1" "fd_prestat_get"
        (func $fd_prestat_get (param i32 i32) (result i32)))
      (memory (export "memory") 1)

      (func (export "_start") (result i32)
        ;; Try to get prestat for fd 3 (first possible preopened fd)
        (call $fd_prestat_get
          (i32.const 3)   ;; fd
          (i32.const 0)   ;; buf ptr
        )
      )
    )
    "#;

    let wasm = wat_to_wasm(wat);
    let module = parser::parse(&HashMap::new(), "test", &mut Reader::new(wasm)).expect("Failed to parse module");

    let ctx = Arc::new(WasiContext::builder().build());

    let mut store = Store::new();
    let imports = create_wasi_imports(&mut store, ctx.clone());
    let instance_id = store
        .create_instance(&module, Some(&imports))
        .expect("Failed to create instance");

    let memory_addr = store
        .get_instance(instance_id)
        .unwrap()
        .get_memory_addr(MEMORY_EXPORT)
        .expect("Failed to get memory");
    ctx.bind_memory(store.get_memory(memory_addr).unwrap().clone());

    let result = store.invoke_export(instance_id, "_start", vec![], None);
    assert!(result.is_ok(), "Execution failed: {:?}", result);

    // Function returns errno, which should be EBADF (8)
    let values = result.unwrap();
    assert_eq!(values.len(), 1);
    if let kasm::runtime::Value::I32(errno) = values[0] {
        assert_eq!(errno, 8, "Expected EBADF (8) for no preopened fds");
    } else {
        panic!("Expected i32 return value");
    }
}

#[test]
fn test_preopen_enumerate() {
    // fd_prestat_get(3) succeeds with a preopen, fd_prestat_get(4) returns EBADF
    let wat = r#"
    (module
      (import "wasi_snapshot_preview1" "fd_prestat_get"
        (func $fd_prestat_get (param i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "fd_prestat_dir_name"
        (func $fd_prestat_dir_name (param i32 i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "fd_write"
        (func $fd_write (param i32 i32 i32 i32) (result i32)))
      (memory (export "memory") 1)

      (func (export "_start") (result i32)
        (local $err i32)
        (local $name_len i32)

        ;; Try fd_prestat_get(3, buf=0)
        (local.set $err (call $fd_prestat_get (i32.const 3) (i32.const 0)))

        ;; If error, return it
        (if (local.get $err)
          (then (return (local.get $err)))
        )

        ;; Read name_len from prestat struct (offset 4)
        (local.set $name_len (i32.load (i32.const 4)))

        ;; Get the dir name into buffer at 100
        (local.set $err (call $fd_prestat_dir_name (i32.const 3) (i32.const 100) (local.get $name_len)))
        (if (local.get $err)
          (then (return (i32.const 999)))
        )

        ;; Write the dir name to stdout
        (i32.store (i32.const 200) (i32.const 100))  ;; iovec ptr
        (i32.store (i32.const 204) (local.get $name_len)) ;; iovec len
        (drop (call $fd_write (i32.const 1) (i32.const 200) (i32.const 1) (i32.const 300)))

        ;; Check that fd 4 returns EBADF
        (call $fd_prestat_get (i32.const 4) (i32.const 0))
      )
    )
    "#;

    let wasm = wat_to_wasm(wat);
    let module = parser::parse(&HashMap::new(), "test", &mut Reader::new(wasm)).expect("Failed to parse module");

    let stdout_buffer = Arc::new(Mutex::new(Vec::<u8>::new()));
    let stdout = CapturedWriter(stdout_buffer.clone());

    let dir = std::env::temp_dir();
    let dir_str = dir.to_str().unwrap();

    let ctx = Arc::new(
        WasiContext::builder()
            .stdout(Box::new(stdout))
            .preopen_dir(dir_str, dir_str)
            .build(),
    );

    let mut store = Store::new();
    let imports = create_wasi_imports(&mut store, ctx.clone());
    let instance_id = store
        .create_instance(&module, Some(&imports))
        .expect("Failed to create instance");

    let memory_addr = store
        .get_instance(instance_id)
        .unwrap()
        .get_memory_addr(MEMORY_EXPORT)
        .expect("Failed to get memory");
    ctx.bind_memory(store.get_memory(memory_addr).unwrap().clone());

    let result = store.invoke_export(instance_id, "_start", vec![], None);
    assert!(result.is_ok(), "Execution failed: {:?}", result);

    // Return value is fd_prestat_get(4) which should be EBADF (8)
    let values = result.unwrap();
    if let kasm::runtime::Value::I32(errno) = values[0] {
        assert_eq!(errno, 8, "Expected EBADF for fd 4");
    } else {
        panic!("Expected i32 return value");
    }

    // Stdout should contain the preopen dir name
    let output = stdout_buffer.lock().unwrap();
    let output_str = String::from_utf8_lossy(&output);
    assert_eq!(output_str, dir_str);
}

#[test]
fn test_path_open_and_read() {
    // Open a file via path_open, read its contents, write to stdout
    let wat = r#"
    (module
      (import "wasi_snapshot_preview1" "fd_prestat_get"
        (func $fd_prestat_get (param i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "path_open"
        (func $path_open (param i32 i32 i32 i32 i32 i64 i64 i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "fd_read"
        (func $fd_read (param i32 i32 i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "fd_write"
        (func $fd_write (param i32 i32 i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "fd_close"
        (func $fd_close (param i32) (result i32)))
      (memory (export "memory") 1)

      ;; Filename at offset 500 (written by test setup via data segment won't work,
      ;; we'll use args instead). We'll hardcode "test_read.txt" = 13 bytes.
      (data (i32.const 500) "test_read.txt")

      (func (export "_start") (result i32)
        (local $err i32)
        (local $file_fd i32)
        (local $nread i32)

        ;; Verify preopen exists
        (local.set $err (call $fd_prestat_get (i32.const 3) (i32.const 0)))
        (if (local.get $err)
          (then (return (i32.const 100)))
        )

        ;; path_open(dir_fd=3, dirflags=0, path=500, path_len=13, oflags=0,
        ;;   rights_base=all, rights_inheriting=all, fdflags=0, opened_fd_ptr=400)
        (local.set $err (call $path_open
          (i32.const 3)           ;; dir fd
          (i32.const 0)           ;; dirflags
          (i32.const 500)         ;; path ptr
          (i32.const 13)          ;; path len
          (i32.const 0)           ;; oflags
          (i64.const 0x1fffffff)  ;; rights_base
          (i64.const 0x1fffffff)  ;; rights_inheriting
          (i32.const 0)           ;; fdflags
          (i32.const 400)         ;; opened_fd ptr
        ))
        (if (local.get $err)
          (then (return (i32.add (i32.const 200) (local.get $err))))
        )

        (local.set $file_fd (i32.load (i32.const 400)))

        ;; Read from file: iovec at 0, buffer at 100
        (i32.store (i32.const 0) (i32.const 100))  ;; buf ptr
        (i32.store (i32.const 4) (i32.const 256))   ;; buf len
        (local.set $err (call $fd_read
          (local.get $file_fd)
          (i32.const 0)    ;; iovs ptr
          (i32.const 1)    ;; iovs count
          (i32.const 300)  ;; nread ptr
        ))
        (if (local.get $err)
          (then (return (i32.const 300)))
        )

        (local.set $nread (i32.load (i32.const 300)))

        ;; Write read data to stdout
        (i32.store (i32.const 0) (i32.const 100))        ;; buf ptr
        (i32.store (i32.const 4) (local.get $nread))       ;; buf len
        (drop (call $fd_write
          (i32.const 1)    ;; stdout
          (i32.const 0)    ;; iovs ptr
          (i32.const 1)    ;; iovs count
          (i32.const 304)  ;; nwritten ptr
        ))

        ;; Close the file
        (drop (call $fd_close (local.get $file_fd)))

        ;; Return 0 for success
        (i32.const 0)
      )
    )
    "#;

    let wasm = wat_to_wasm(wat);
    let module = parser::parse(&HashMap::new(), "test", &mut Reader::new(wasm)).expect("Failed to parse module");

    // Create a temp file with known content
    let dir = std::env::temp_dir();
    let file_path = dir.join("test_read.txt");
    std::fs::write(&file_path, "file contents here").expect("Failed to write test file");

    let stdout_buffer = Arc::new(Mutex::new(Vec::<u8>::new()));
    let stdout = CapturedWriter(stdout_buffer.clone());
    let dir_str = dir.to_str().unwrap();

    let ctx = Arc::new(
        WasiContext::builder()
            .stdout(Box::new(stdout))
            .preopen_dir(dir_str, dir_str)
            .build(),
    );

    let mut store = Store::new();
    let imports = create_wasi_imports(&mut store, ctx.clone());
    let instance_id = store
        .create_instance(&module, Some(&imports))
        .expect("Failed to create instance");

    let memory_addr = store
        .get_instance(instance_id)
        .unwrap()
        .get_memory_addr(MEMORY_EXPORT)
        .expect("Failed to get memory");
    ctx.bind_memory(store.get_memory(memory_addr).unwrap().clone());

    let result = store.invoke_export(instance_id, "_start", vec![], None);
    assert!(result.is_ok(), "Execution failed: {:?}", result);

    let values = result.unwrap();
    if let kasm::runtime::Value::I32(ret) = values[0] {
        assert_eq!(ret, 0, "Expected success (0), got {}", ret);
    }

    let output = stdout_buffer.lock().unwrap();
    assert_eq!(&*output, b"file contents here");

    // Clean up
    let _ = std::fs::remove_file(&file_path);
}

#[test]
fn test_path_open_write() {
    // Open a file for writing via path_open with O_CREAT
    let wat = r#"
    (module
      (import "wasi_snapshot_preview1" "fd_prestat_get"
        (func $fd_prestat_get (param i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "path_open"
        (func $path_open (param i32 i32 i32 i32 i32 i64 i64 i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "fd_write"
        (func $fd_write (param i32 i32 i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "fd_close"
        (func $fd_close (param i32) (result i32)))
      (memory (export "memory") 1)

      ;; Filename
      (data (i32.const 500) "test_write.txt")
      ;; Data to write
      (data (i32.const 600) "written by wasm")

      (func (export "_start") (result i32)
        (local $err i32)
        (local $file_fd i32)

        ;; Verify preopen
        (local.set $err (call $fd_prestat_get (i32.const 3) (i32.const 0)))
        (if (local.get $err) (then (return (i32.const 100))))

        ;; path_open with O_CREAT (1) | O_TRUNC (8) = 9
        (local.set $err (call $path_open
          (i32.const 3) (i32.const 0) (i32.const 500) (i32.const 14)
          (i32.const 9)
          (i64.const 0x1fffffff) (i64.const 0x1fffffff)
          (i32.const 0) (i32.const 400)
        ))
        (if (local.get $err) (then (return (i32.add (i32.const 200) (local.get $err)))))

        (local.set $file_fd (i32.load (i32.const 400)))

        ;; Write data to file
        (i32.store (i32.const 0) (i32.const 600))   ;; buf ptr
        (i32.store (i32.const 4) (i32.const 15))     ;; len = "written by wasm"
        (local.set $err (call $fd_write
          (local.get $file_fd)
          (i32.const 0) (i32.const 1) (i32.const 300)
        ))
        (if (local.get $err) (then (return (i32.const 300))))

        ;; Close
        (drop (call $fd_close (local.get $file_fd)))

        (i32.const 0)
      )
    )
    "#;

    let wasm = wat_to_wasm(wat);
    let module = parser::parse(&HashMap::new(), "test", &mut Reader::new(wasm)).expect("Failed to parse module");

    let dir = std::env::temp_dir();
    let file_path = dir.join("test_write.txt");
    // Remove if exists from previous run
    let _ = std::fs::remove_file(&file_path);

    let dir_str = dir.to_str().unwrap();
    let ctx = Arc::new(WasiContext::builder().preopen_dir(dir_str, dir_str).build());

    let mut store = Store::new();
    let imports = create_wasi_imports(&mut store, ctx.clone());
    let instance_id = store
        .create_instance(&module, Some(&imports))
        .expect("Failed to create instance");

    let memory_addr = store
        .get_instance(instance_id)
        .unwrap()
        .get_memory_addr(MEMORY_EXPORT)
        .expect("Failed to get memory");
    ctx.bind_memory(store.get_memory(memory_addr).unwrap().clone());

    let result = store.invoke_export(instance_id, "_start", vec![], None);
    assert!(result.is_ok(), "Execution failed: {:?}", result);

    let values = result.unwrap();
    if let kasm::runtime::Value::I32(ret) = values[0] {
        assert_eq!(ret, 0, "Expected success (0), got {}", ret);
    }

    // Verify file was written on host
    let contents = std::fs::read_to_string(&file_path).expect("Failed to read written file");
    assert_eq!(contents, "written by wasm");

    // Clean up
    let _ = std::fs::remove_file(&file_path);
}

#[test]
fn test_fd_close_then_read_fails() {
    // Open a file, close it, then attempt to read (should return EBADF)
    let wat = r#"
    (module
      (import "wasi_snapshot_preview1" "fd_prestat_get"
        (func $fd_prestat_get (param i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "path_open"
        (func $path_open (param i32 i32 i32 i32 i32 i64 i64 i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "fd_read"
        (func $fd_read (param i32 i32 i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "fd_close"
        (func $fd_close (param i32) (result i32)))
      (memory (export "memory") 1)

      (data (i32.const 500) "test_close.txt")

      (func (export "_start") (result i32)
        (local $file_fd i32)
        (local $err i32)

        ;; Verify preopen
        (local.set $err (call $fd_prestat_get (i32.const 3) (i32.const 0)))
        (if (local.get $err) (then (return (i32.const 100))))

        ;; Open file
        (local.set $err (call $path_open
          (i32.const 3) (i32.const 0) (i32.const 500) (i32.const 14)
          (i32.const 0) (i64.const 0x1fffffff) (i64.const 0x1fffffff)
          (i32.const 0) (i32.const 400)
        ))
        (if (local.get $err) (then (return (i32.const 200))))

        (local.set $file_fd (i32.load (i32.const 400)))

        ;; Close the file
        (drop (call $fd_close (local.get $file_fd)))

        ;; Try to read from closed fd
        (i32.store (i32.const 0) (i32.const 100))
        (i32.store (i32.const 4) (i32.const 64))
        (call $fd_read (local.get $file_fd) (i32.const 0) (i32.const 1) (i32.const 300))
        ;; Should return EBADF (8)
      )
    )
    "#;

    let wasm = wat_to_wasm(wat);
    let module = parser::parse(&HashMap::new(), "test", &mut Reader::new(wasm)).expect("Failed to parse module");

    let dir = std::env::temp_dir();
    let file_path = dir.join("test_close.txt");
    std::fs::write(&file_path, "data").expect("Failed to write test file");

    let dir_str = dir.to_str().unwrap();
    let ctx = Arc::new(WasiContext::builder().preopen_dir(dir_str, dir_str).build());

    let mut store = Store::new();
    let imports = create_wasi_imports(&mut store, ctx.clone());
    let instance_id = store
        .create_instance(&module, Some(&imports))
        .expect("Failed to create instance");

    let memory_addr = store
        .get_instance(instance_id)
        .unwrap()
        .get_memory_addr(MEMORY_EXPORT)
        .expect("Failed to get memory");
    ctx.bind_memory(store.get_memory(memory_addr).unwrap().clone());

    let result = store.invoke_export(instance_id, "_start", vec![], None);
    assert!(result.is_ok(), "Execution failed: {:?}", result);

    let values = result.unwrap();
    if let kasm::runtime::Value::I32(errno) = values[0] {
        assert_eq!(errno, 8, "Expected EBADF (8) for closed fd, got {}", errno);
    }

    let _ = std::fs::remove_file(&file_path);
}

#[test]
fn test_path_traversal_blocked() {
    // Attempt to open ../../../etc/passwd should return EACCES
    let wat = r#"
    (module
      (import "wasi_snapshot_preview1" "fd_prestat_get"
        (func $fd_prestat_get (param i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "path_open"
        (func $path_open (param i32 i32 i32 i32 i32 i64 i64 i32 i32) (result i32)))
      (memory (export "memory") 1)

      ;; Traversal path
      (data (i32.const 500) "../../../etc/passwd")

      (func (export "_start") (result i32)
        (local $err i32)

        ;; Verify preopen
        (local.set $err (call $fd_prestat_get (i32.const 3) (i32.const 0)))
        (if (local.get $err) (then (return (i32.const 100))))

        ;; Try path traversal
        (call $path_open
          (i32.const 3) (i32.const 0) (i32.const 500) (i32.const 19)
          (i32.const 0) (i64.const 0x1fffffff) (i64.const 0x1fffffff)
          (i32.const 0) (i32.const 400)
        )
        ;; Should return EACCES (2)
      )
    )
    "#;

    let wasm = wat_to_wasm(wat);
    let module = parser::parse(&HashMap::new(), "test", &mut Reader::new(wasm)).expect("Failed to parse module");

    let dir = std::env::temp_dir();
    let dir_str = dir.to_str().unwrap();
    let ctx = Arc::new(WasiContext::builder().preopen_dir(dir_str, dir_str).build());

    let mut store = Store::new();
    let imports = create_wasi_imports(&mut store, ctx.clone());
    let instance_id = store
        .create_instance(&module, Some(&imports))
        .expect("Failed to create instance");

    let memory_addr = store
        .get_instance(instance_id)
        .unwrap()
        .get_memory_addr(MEMORY_EXPORT)
        .expect("Failed to get memory");
    ctx.bind_memory(store.get_memory(memory_addr).unwrap().clone());

    let result = store.invoke_export(instance_id, "_start", vec![], None);
    assert!(result.is_ok(), "Execution failed: {:?}", result);

    let values = result.unwrap();
    if let kasm::runtime::Value::I32(errno) = values[0] {
        // Should be EACCES (2) for path traversal
        assert_eq!(errno, 2, "Expected EACCES (2) for path traversal, got {}", errno);
    }
}

#[test]
fn test_path_open_nonexistent_file() {
    // Opening a file that doesn't exist without O_CREAT returns ENOENT
    let wat = r#"
    (module
      (import "wasi_snapshot_preview1" "fd_prestat_get"
        (func $fd_prestat_get (param i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "path_open"
        (func $path_open (param i32 i32 i32 i32 i32 i64 i64 i32 i32) (result i32)))
      (memory (export "memory") 1)

      (data (i32.const 500) "no_such_file.txt")

      (func (export "_start") (result i32)
        (local $err i32)
        (local.set $err (call $fd_prestat_get (i32.const 3) (i32.const 0)))
        (if (local.get $err) (then (return (i32.const 100))))

        (call $path_open
          (i32.const 3) (i32.const 0) (i32.const 500) (i32.const 16)
          (i32.const 0) (i64.const 0x1fffffff) (i64.const 0x1fffffff)
          (i32.const 0) (i32.const 400)
        )
      )
    )
    "#;

    let wasm = wat_to_wasm(wat);
    let module = parser::parse(&HashMap::new(), "test", &mut Reader::new(wasm)).expect("Failed to parse module");

    let dir = std::env::temp_dir();
    let dir_str = dir.to_str().unwrap();
    let ctx = Arc::new(WasiContext::builder().preopen_dir(dir_str, dir_str).build());

    let mut store = Store::new();
    let imports = create_wasi_imports(&mut store, ctx.clone());
    let instance_id = store
        .create_instance(&module, Some(&imports))
        .expect("Failed to create instance");

    let memory_addr = store
        .get_instance(instance_id)
        .unwrap()
        .get_memory_addr(MEMORY_EXPORT)
        .expect("Failed to get memory");
    ctx.bind_memory(store.get_memory(memory_addr).unwrap().clone());

    let result = store.invoke_export(instance_id, "_start", vec![], None);
    assert!(result.is_ok(), "Execution failed: {:?}", result);

    let values = result.unwrap();
    if let kasm::runtime::Value::I32(errno) = values[0] {
        assert_eq!(errno, 44, "Expected ENOENT (44), got {}", errno);
    }
}

#[test]
fn test_fd_seek() {
    // Open a file, seek to offset 6, read from there
    let wat = r#"
    (module
      (import "wasi_snapshot_preview1" "fd_prestat_get"
        (func $fd_prestat_get (param i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "path_open"
        (func $path_open (param i32 i32 i32 i32 i32 i64 i64 i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "fd_read"
        (func $fd_read (param i32 i32 i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "fd_seek"
        (func $fd_seek (param i32 i64 i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "fd_write"
        (func $fd_write (param i32 i32 i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "fd_close"
        (func $fd_close (param i32) (result i32)))
      (memory (export "memory") 1)

      (data (i32.const 500) "test_seek.txt")

      (func (export "_start") (result i32)
        (local $err i32)
        (local $file_fd i32)
        (local $nread i32)

        (local.set $err (call $fd_prestat_get (i32.const 3) (i32.const 0)))
        (if (local.get $err) (then (return (i32.const 100))))

        ;; Open file
        (local.set $err (call $path_open
          (i32.const 3) (i32.const 0) (i32.const 500) (i32.const 13)
          (i32.const 0) (i64.const 0x1fffffff) (i64.const 0x1fffffff)
          (i32.const 0) (i32.const 400)
        ))
        (if (local.get $err) (then (return (i32.add (i32.const 200) (local.get $err)))))
        (local.set $file_fd (i32.load (i32.const 400)))

        ;; Seek to offset 6 (SEEK_SET = 0)
        (local.set $err (call $fd_seek
          (local.get $file_fd)
          (i64.const 6)
          (i32.const 0)    ;; SEEK_SET
          (i32.const 410)  ;; newoffset ptr
        ))
        (if (local.get $err) (then (return (i32.const 300))))

        ;; Read remaining bytes
        (i32.store (i32.const 0) (i32.const 100))
        (i32.store (i32.const 4) (i32.const 256))
        (local.set $err (call $fd_read
          (local.get $file_fd) (i32.const 0) (i32.const 1) (i32.const 420)
        ))
        (if (local.get $err) (then (return (i32.const 400))))
        (local.set $nread (i32.load (i32.const 420)))

        ;; Write to stdout
        (i32.store (i32.const 0) (i32.const 100))
        (i32.store (i32.const 4) (local.get $nread))
        (drop (call $fd_write (i32.const 1) (i32.const 0) (i32.const 1) (i32.const 430)))

        (drop (call $fd_close (local.get $file_fd)))
        (i32.const 0)
      )
    )
    "#;

    let wasm = wat_to_wasm(wat);
    let module = parser::parse(&HashMap::new(), "test", &mut Reader::new(wasm)).expect("Failed to parse module");

    let dir = std::env::temp_dir();
    let file_path = dir.join("test_seek.txt");
    std::fs::write(&file_path, "Hello,World!").expect("Failed to write test file");

    let stdout_buffer = Arc::new(Mutex::new(Vec::<u8>::new()));
    let stdout = CapturedWriter(stdout_buffer.clone());
    let dir_str = dir.to_str().unwrap();

    let ctx = Arc::new(
        WasiContext::builder()
            .stdout(Box::new(stdout))
            .preopen_dir(dir_str, dir_str)
            .build(),
    );

    let mut store = Store::new();
    let imports = create_wasi_imports(&mut store, ctx.clone());
    let instance_id = store
        .create_instance(&module, Some(&imports))
        .expect("Failed to create instance");

    let memory_addr = store
        .get_instance(instance_id)
        .unwrap()
        .get_memory_addr(MEMORY_EXPORT)
        .expect("Failed to get memory");
    ctx.bind_memory(store.get_memory(memory_addr).unwrap().clone());

    let result = store.invoke_export(instance_id, "_start", vec![], None);
    assert!(result.is_ok(), "Execution failed: {:?}", result);

    let values = result.unwrap();
    if let kasm::runtime::Value::I32(ret) = values[0] {
        assert_eq!(ret, 0, "Expected success (0), got {}", ret);
    }

    // After seeking to offset 6, reading "Hello,World!" should give "World!"
    let output = stdout_buffer.lock().unwrap();
    assert_eq!(&*output, b"World!");

    let _ = std::fs::remove_file(&file_path);
}

#[test]
fn test_fd_fdstat_get() {
    // Verify fd_fdstat_get returns correct file types
    let wat = r#"
    (module
      (import "wasi_snapshot_preview1" "fd_prestat_get"
        (func $fd_prestat_get (param i32 i32) (result i32)))
      (import "wasi_snapshot_preview1" "fd_fdstat_get"
        (func $fd_fdstat_get (param i32 i32) (result i32)))
      (memory (export "memory") 1)

      (func (export "_start") (result i32)
        (local $err i32)

        ;; Verify preopen exists
        (local.set $err (call $fd_prestat_get (i32.const 3) (i32.const 0)))
        (if (local.get $err) (then (return (i32.const 100))))

        ;; fd_fdstat_get on stdout (fd 1) -> CharacterDevice (2)
        (local.set $err (call $fd_fdstat_get (i32.const 1) (i32.const 200)))
        (if (local.get $err) (then (return (i32.const 200))))

        ;; fd_fdstat_get on preopen dir (fd 3) -> Directory (3)
        (local.set $err (call $fd_fdstat_get (i32.const 3) (i32.const 300)))
        (if (local.get $err) (then (return (i32.const 300))))

        ;; Return: (stdout_type << 8) | dir_type
        ;; So we can check both in one return value
        (i32.or
          (i32.shl (i32.load8_u (i32.const 200)) (i32.const 8))
          (i32.load8_u (i32.const 300))
        )
      )
    )
    "#;

    let wasm = wat_to_wasm(wat);
    let module = parser::parse(&HashMap::new(), "test", &mut Reader::new(wasm)).expect("Failed to parse module");

    let dir = std::env::temp_dir();
    let dir_str = dir.to_str().unwrap();
    let ctx = Arc::new(
        WasiContext::builder()
            .stdout(Box::new(std::io::sink()))
            .preopen_dir(dir_str, dir_str)
            .build(),
    );

    let mut store = Store::new();
    let imports = create_wasi_imports(&mut store, ctx.clone());
    let instance_id = store
        .create_instance(&module, Some(&imports))
        .expect("Failed to create instance");

    let memory_addr = store
        .get_instance(instance_id)
        .unwrap()
        .get_memory_addr(MEMORY_EXPORT)
        .expect("Failed to get memory");
    ctx.bind_memory(store.get_memory(memory_addr).unwrap().clone());

    let result = store.invoke_export(instance_id, "_start", vec![], None);
    assert!(result.is_ok(), "Execution failed: {:?}", result);

    let values = result.unwrap();
    if let kasm::runtime::Value::I32(combined) = values[0] {
        let stdout_type = (combined >> 8) & 0xff;
        let dir_type = combined & 0xff;
        assert_eq!(
            stdout_type, 2,
            "Expected CharacterDevice (2) for stdout, got {}",
            stdout_type
        );
        assert_eq!(dir_type, 3, "Expected Directory (3) for preopen, got {}", dir_type);
    } else {
        panic!("Expected i32 return value");
    }
}

#[test]
fn test_return_from_nested_block_preserves_caller() {
    // This tests the fix for a bug where returning from within a nested block
    // did not properly pop the call frame, causing the caller's execution
    // context to be corrupted.
    //
    // The WAT creates two functions:
    // - helper: contains nested blocks and returns 42 from inside them
    // - main: stores 100 in local, calls helper, returns its local
    //
    // Before the fix, main would return garbage because its execution context
    // was corrupted by helper's return not cleaning up properly.

    let wat = r#"
    (module
      ;; Helper function: returns 42 from within nested blocks
      (func $helper (result i32)
        (block
          (block
            (i32.const 42)
            (return)
          )
        )
        (i32.const 0)  ;; unreachable
      )

      ;; Main function: stores value in local, calls helper, returns local
      (func (export "_start") (result i32)
        (local $value i32)
        (local.set $value (i32.const 100))
        (drop (call $helper))
        (local.get $value)
      )
    )
    "#;

    let wasm = wat_to_wasm(wat);
    let module = parser::parse(&HashMap::new(), "test", &mut Reader::new(wasm)).expect("Failed to parse module");

    let mut store = Store::new();
    let instance_id = store.create_instance(&module, None).expect("Failed to create instance");

    let result = store.invoke_export(instance_id, "_start", vec![], None);
    assert!(result.is_ok(), "Execution failed: {:?}", result);

    let values = result.unwrap();
    if let kasm::runtime::Value::I32(v) = values[0] {
        assert_eq!(v, 100, "Expected caller's local (100) to be preserved, got {}", v);
    } else {
        panic!("Expected i32 return value");
    }
}
