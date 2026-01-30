//! WASI (WebAssembly System Interface) preview1 implementation
//!
//! This module provides WASI preview1 support for stdin, stdout, stderr, and argv
//! to enable CLI-style WebAssembly programs.
//!
//! # Specification
//!
//! This implementation follows the WASI preview1 specification:
//! <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md>
//!
//! # Architecture
//!
//! WASI functions are closures that capture `Arc<WasiContext>`. Memory is bound
//! to the context after module instantiation via `bind_memory()`.
//!
//! # Usage
//!
//! ```ignore
//! use kasm::runtime::wasi::{WasiContext, create_wasi_imports};
//! use std::sync::Arc;
//!
//! let ctx = Arc::new(WasiContext::builder()
//!     .args(["prog", "arg1"])
//!     .stdin(Box::new(std::io::stdin()))
//!     .stdout(Box::new(std::io::stdout()))
//!     .stderr(Box::new(std::io::stderr()))
//!     .build());
//!
//! let imports = create_wasi_imports(&mut store, ctx.clone());
//! let instance_id = store.create_instance(&module, Some(&imports))?;
//!
//! // Bind memory after instantiation
//! let memory = store.get_instance(instance_id).unwrap()
//!     .get_memory_addr("memory")?;
//! ctx.bind_memory(store.get_memory(memory).unwrap().clone());
//!
//! // Execute the module
//! store.invoke_export(instance_id, "_start", vec![], None)?;
//! ```

pub mod assemblyscript;
pub mod context;
pub mod types;

pub use assemblyscript::add_assemblyscript_imports;
pub use context::{FileDescriptor, WasiContext, WasiContextBuilder};
pub use types::WasiErrno;

use crate::parser::module::{FunctionType, Module, ValueType};
use crate::runtime::store::{FunctionInstance, Store};
use crate::runtime::{ImportObject, RuntimeError, Value};
use std::sync::Arc;

/// Standard export name for WebAssembly linear memory.
pub const MEMORY_EXPORT: &str = "memory";

/// Create a WASI instance with all necessary setup.
///
/// This is a convenience function that:
/// 1. Creates WASI imports
/// 2. Optionally adds AssemblyScript imports
/// 3. Creates the module instance
/// 4. Binds memory to the WASI context
///
/// # Arguments
///
/// * `store` - The WebAssembly store
/// * `module` - The parsed WebAssembly module
/// * `ctx` - The WASI context (wrapped in Arc for sharing)
/// * `include_assemblyscript` - Whether to include AssemblyScript's env.abort
///
/// # Returns
///
/// The instance ID on success, or a RuntimeError on failure.
pub fn create_wasi_instance<'a>(
    store: &mut Store<'a>,
    module: &'a Module,
    ctx: Arc<WasiContext>,
    include_assemblyscript: bool,
) -> Result<usize, RuntimeError> {
    let mut imports = create_wasi_imports(store, ctx.clone());

    if include_assemblyscript {
        add_assemblyscript_imports(store, &mut imports, ctx.clone());
    }

    let instance_id = store.create_instance(module, Some(&imports))?;

    // Bind memory to WASI context
    if let Some(instance) = store.get_instance(instance_id)
        && let Ok(memory_addr) = instance.get_memory_addr(MEMORY_EXPORT)
        && let Some(memory) = store.get_memory(memory_addr)
    {
        ctx.bind_memory(memory.clone());
    }

    Ok(instance_id)
}

/// Extract an i32 value from a WASI function argument at the given index.
fn extract_i32(args: &[Value], index: usize) -> Result<i32, RuntimeError> {
    match args.get(index) {
        Some(Value::I32(v)) => Ok(*v),
        other => Err(RuntimeError::TypeMismatch {
            expected: "i32".to_string(),
            actual: format!("{:?}", other),
        }),
    }
}

/// Extract an i64 value from a WASI function argument at the given index.
fn extract_i64(args: &[Value], index: usize) -> Result<i64, RuntimeError> {
    match args.get(index) {
        Some(Value::I64(v)) => Ok(*v),
        other => Err(RuntimeError::TypeMismatch {
            expected: "i64".to_string(),
            actual: format!("{:?}", other),
        }),
    }
}

/// Create WASI import functions and register them in the store
///
/// Returns an ImportObject with all WASI functions registered.
pub fn create_wasi_imports(store: &mut Store<'_>, ctx: Arc<WasiContext>) -> ImportObject {
    let mut imports = ImportObject::new();

    // fd_write: (fd, iovs_ptr, iovs_len, nwritten_ptr) -> errno
    let fd_write_type = FunctionType {
        parameters: vec![ValueType::I32, ValueType::I32, ValueType::I32, ValueType::I32],
        return_types: vec![ValueType::I32],
    };
    let ctx_fd_write = ctx.clone();
    let fd_write_addr = store.allocate_function(FunctionInstance::Host {
        func: Box::new(move |args| wasi_fd_write(&ctx_fd_write, args)),
        func_type: fd_write_type,
    });
    imports.add_function("wasi_snapshot_preview1", "fd_write", fd_write_addr);

    // fd_read: (fd, iovs_ptr, iovs_len, nread_ptr) -> errno
    let fd_read_type = FunctionType {
        parameters: vec![ValueType::I32, ValueType::I32, ValueType::I32, ValueType::I32],
        return_types: vec![ValueType::I32],
    };
    let ctx_fd_read = ctx.clone();
    let fd_read_addr = store.allocate_function(FunctionInstance::Host {
        func: Box::new(move |args| wasi_fd_read(&ctx_fd_read, args)),
        func_type: fd_read_type,
    });
    imports.add_function("wasi_snapshot_preview1", "fd_read", fd_read_addr);

    // fd_close: (fd) -> errno
    let fd_close_type = FunctionType {
        parameters: vec![ValueType::I32],
        return_types: vec![ValueType::I32],
    };
    let ctx_fd_close = ctx.clone();
    let fd_close_addr = store.allocate_function(FunctionInstance::Host {
        func: Box::new(move |args| wasi_fd_close(&ctx_fd_close, args)),
        func_type: fd_close_type,
    });
    imports.add_function("wasi_snapshot_preview1", "fd_close", fd_close_addr);

    // args_sizes_get: (argc_ptr, argv_buf_size_ptr) -> errno
    let args_sizes_get_type = FunctionType {
        parameters: vec![ValueType::I32, ValueType::I32],
        return_types: vec![ValueType::I32],
    };
    let ctx_args_sizes = ctx.clone();
    let args_sizes_get_addr = store.allocate_function(FunctionInstance::Host {
        func: Box::new(move |args| wasi_args_sizes_get(&ctx_args_sizes, args)),
        func_type: args_sizes_get_type,
    });
    imports.add_function("wasi_snapshot_preview1", "args_sizes_get", args_sizes_get_addr);

    // args_get: (argv_ptr, argv_buf_ptr) -> errno
    let args_get_type = FunctionType {
        parameters: vec![ValueType::I32, ValueType::I32],
        return_types: vec![ValueType::I32],
    };
    let ctx_args_get = ctx.clone();
    let args_get_addr = store.allocate_function(FunctionInstance::Host {
        func: Box::new(move |args| wasi_args_get(&ctx_args_get, args)),
        func_type: args_get_type,
    });
    imports.add_function("wasi_snapshot_preview1", "args_get", args_get_addr);

    // environ_sizes_get: (environc_ptr, environ_buf_size_ptr) -> errno
    let environ_sizes_get_type = FunctionType {
        parameters: vec![ValueType::I32, ValueType::I32],
        return_types: vec![ValueType::I32],
    };
    let ctx_environ_sizes = ctx.clone();
    let environ_sizes_get_addr = store.allocate_function(FunctionInstance::Host {
        func: Box::new(move |args| wasi_environ_sizes_get(&ctx_environ_sizes, args)),
        func_type: environ_sizes_get_type,
    });
    imports.add_function("wasi_snapshot_preview1", "environ_sizes_get", environ_sizes_get_addr);

    // environ_get: (environ_ptr, environ_buf_ptr) -> errno
    let environ_get_type = FunctionType {
        parameters: vec![ValueType::I32, ValueType::I32],
        return_types: vec![ValueType::I32],
    };
    let ctx_environ_get = ctx.clone();
    let environ_get_addr = store.allocate_function(FunctionInstance::Host {
        func: Box::new(move |args| wasi_environ_get(&ctx_environ_get, args)),
        func_type: environ_get_type,
    });
    imports.add_function("wasi_snapshot_preview1", "environ_get", environ_get_addr);

    // fd_prestat_get: (fd, buf_ptr) -> errno
    let fd_prestat_get_type = FunctionType {
        parameters: vec![ValueType::I32, ValueType::I32],
        return_types: vec![ValueType::I32],
    };
    let ctx_prestat_get = ctx.clone();
    let fd_prestat_get_addr = store.allocate_function(FunctionInstance::Host {
        func: Box::new(move |args| wasi_fd_prestat_get(&ctx_prestat_get, args)),
        func_type: fd_prestat_get_type,
    });
    imports.add_function("wasi_snapshot_preview1", "fd_prestat_get", fd_prestat_get_addr);

    // fd_prestat_dir_name: (fd, path_ptr, path_len) -> errno
    let fd_prestat_dir_name_type = FunctionType {
        parameters: vec![ValueType::I32, ValueType::I32, ValueType::I32],
        return_types: vec![ValueType::I32],
    };
    let ctx_prestat_dir_name = ctx.clone();
    let fd_prestat_dir_name_addr = store.allocate_function(FunctionInstance::Host {
        func: Box::new(move |args| wasi_fd_prestat_dir_name(&ctx_prestat_dir_name, args)),
        func_type: fd_prestat_dir_name_type,
    });
    imports.add_function(
        "wasi_snapshot_preview1",
        "fd_prestat_dir_name",
        fd_prestat_dir_name_addr,
    );

    // path_open: (fd, dirflags, path_ptr, path_len, oflags, rights_base, rights_inheriting, fdflags, opened_fd_ptr) -> errno
    let path_open_type = FunctionType {
        parameters: vec![
            ValueType::I32,
            ValueType::I32,
            ValueType::I32,
            ValueType::I32,
            ValueType::I32,
            ValueType::I64,
            ValueType::I64,
            ValueType::I32,
            ValueType::I32,
        ],
        return_types: vec![ValueType::I32],
    };
    let ctx_path_open = ctx.clone();
    let path_open_addr = store.allocate_function(FunctionInstance::Host {
        func: Box::new(move |args| wasi_path_open(&ctx_path_open, args)),
        func_type: path_open_type,
    });
    imports.add_function("wasi_snapshot_preview1", "path_open", path_open_addr);

    // fd_fdstat_get: (fd, buf_ptr) -> errno
    let fd_fdstat_get_type = FunctionType {
        parameters: vec![ValueType::I32, ValueType::I32],
        return_types: vec![ValueType::I32],
    };
    let ctx_fdstat_get = ctx.clone();
    let fd_fdstat_get_addr = store.allocate_function(FunctionInstance::Host {
        func: Box::new(move |args| wasi_fd_fdstat_get(&ctx_fdstat_get, args)),
        func_type: fd_fdstat_get_type,
    });
    imports.add_function("wasi_snapshot_preview1", "fd_fdstat_get", fd_fdstat_get_addr);

    // fd_seek: (fd, offset, whence, newoffset_ptr) -> errno
    let fd_seek_type = FunctionType {
        parameters: vec![ValueType::I32, ValueType::I64, ValueType::I32, ValueType::I32],
        return_types: vec![ValueType::I32],
    };
    let ctx_fd_seek = ctx.clone();
    let fd_seek_addr = store.allocate_function(FunctionInstance::Host {
        func: Box::new(move |args| wasi_fd_seek(&ctx_fd_seek, args)),
        func_type: fd_seek_type,
    });
    imports.add_function("wasi_snapshot_preview1", "fd_seek", fd_seek_addr);

    // proc_exit: (code) -> !
    let proc_exit_type = FunctionType {
        parameters: vec![ValueType::I32],
        return_types: vec![],
    };
    let ctx_proc_exit = ctx;
    let proc_exit_addr = store.allocate_function(FunctionInstance::Host {
        func: Box::new(move |args| wasi_proc_exit(&ctx_proc_exit, args)),
        func_type: proc_exit_type,
    });
    imports.add_function("wasi_snapshot_preview1", "proc_exit", proc_exit_addr);

    imports
}

// === WASI function implementations ===

/// fd_write: Write to a file descriptor using scatter/gather I/O.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#fd_write>
fn wasi_fd_write(ctx: &WasiContext, args: Vec<Value>) -> Result<Vec<Value>, RuntimeError> {
    let fd = extract_i32(&args, 0)? as u32;
    let iovs_ptr = extract_i32(&args, 1)? as u32;
    let iovs_len = extract_i32(&args, 2)? as u32;
    let nwritten_ptr = extract_i32(&args, 3)? as u32;

    match ctx.fd_write(fd, iovs_ptr, iovs_len) {
        Ok(nwritten) => {
            ctx.write_u32(nwritten_ptr, nwritten as u32)?;
            Ok(vec![Value::I32(WasiErrno::Success.as_u32() as i32)])
        }
        Err(errno) => Ok(vec![Value::I32(errno.as_u32() as i32)]),
    }
}

/// fd_read: Read from a file descriptor using scatter/gather I/O.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#fd_read>
fn wasi_fd_read(ctx: &WasiContext, args: Vec<Value>) -> Result<Vec<Value>, RuntimeError> {
    let fd = extract_i32(&args, 0)? as u32;
    let iovs_ptr = extract_i32(&args, 1)? as u32;
    let iovs_len = extract_i32(&args, 2)? as u32;
    let nread_ptr = extract_i32(&args, 3)? as u32;

    match ctx.fd_read(fd, iovs_ptr, iovs_len) {
        Ok(nread) => {
            ctx.write_u32(nread_ptr, nread as u32)?;
            Ok(vec![Value::I32(WasiErrno::Success.as_u32() as i32)])
        }
        Err(errno) => Ok(vec![Value::I32(errno.as_u32() as i32)]),
    }
}

/// fd_close: Close a file descriptor.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#fd_close>
fn wasi_fd_close(ctx: &WasiContext, args: Vec<Value>) -> Result<Vec<Value>, RuntimeError> {
    let fd = extract_i32(&args, 0)? as u32;

    match ctx.close_fd(fd) {
        Ok(()) => Ok(vec![Value::I32(WasiErrno::Success.as_u32() as i32)]),
        Err(errno) => Ok(vec![Value::I32(errno.as_u32() as i32)]),
    }
}

/// args_sizes_get: Return the number of arguments and the size of the argument string data.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#args_sizes_get>
fn wasi_args_sizes_get(ctx: &WasiContext, args: Vec<Value>) -> Result<Vec<Value>, RuntimeError> {
    let argc_ptr = extract_i32(&args, 0)? as u32;
    let argv_buf_size_ptr = extract_i32(&args, 1)? as u32;

    let argc = ctx.args().len() as u32;
    // Each arg is null-terminated, so +1 for each
    let argv_buf_size: u32 = ctx.args().iter().map(|s| s.len() as u32 + 1).sum();

    ctx.write_u32(argc_ptr, argc)?;
    ctx.write_u32(argv_buf_size_ptr, argv_buf_size)?;

    Ok(vec![Value::I32(WasiErrno::Success.as_u32() as i32)])
}

/// args_get: Read command-line argument data into provided buffers.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#args_get>
fn wasi_args_get(ctx: &WasiContext, args: Vec<Value>) -> Result<Vec<Value>, RuntimeError> {
    let argv_ptr = extract_i32(&args, 0)? as u32;
    let argv_buf_ptr = extract_i32(&args, 1)? as u32;

    let mut buf_offset = 0u32;
    for (i, arg) in ctx.args().iter().enumerate() {
        // Write pointer to this arg
        let arg_addr = argv_buf_ptr + buf_offset;
        ctx.write_u32(argv_ptr + (i as u32 * 4), arg_addr)?;

        // Write the arg string (null-terminated)
        let mut arg_bytes = arg.as_bytes().to_vec();
        arg_bytes.push(0); // null terminator
        ctx.write_bytes(arg_addr, &arg_bytes)?;

        buf_offset += arg_bytes.len() as u32;
    }

    Ok(vec![Value::I32(WasiErrno::Success.as_u32() as i32)])
}

/// environ_sizes_get: Return the number of environment variables and the size of the data.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#environ_sizes_get>
fn wasi_environ_sizes_get(ctx: &WasiContext, args: Vec<Value>) -> Result<Vec<Value>, RuntimeError> {
    let environc_ptr = extract_i32(&args, 0)? as u32;
    let environ_buf_size_ptr = extract_i32(&args, 1)? as u32;

    let environc = ctx.env().len() as u32;
    let environ_buf_size: u32 = ctx.env().iter().map(|s| s.len() as u32 + 1).sum();

    ctx.write_u32(environc_ptr, environc)?;
    ctx.write_u32(environ_buf_size_ptr, environ_buf_size)?;

    Ok(vec![Value::I32(WasiErrno::Success.as_u32() as i32)])
}

/// environ_get: Read environment variable data into provided buffers.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#environ_get>
fn wasi_environ_get(ctx: &WasiContext, args: Vec<Value>) -> Result<Vec<Value>, RuntimeError> {
    let environ_ptr = extract_i32(&args, 0)? as u32;
    let environ_buf_ptr = extract_i32(&args, 1)? as u32;

    let mut buf_offset = 0u32;
    for (i, env_var) in ctx.env().iter().enumerate() {
        // Write pointer to this env var
        let env_addr = environ_buf_ptr + buf_offset;
        ctx.write_u32(environ_ptr + (i as u32 * 4), env_addr)?;

        // Write the env var string (null-terminated)
        let mut env_bytes = env_var.as_bytes().to_vec();
        env_bytes.push(0);
        ctx.write_bytes(env_addr, &env_bytes)?;

        buf_offset += env_bytes.len() as u32;
    }

    Ok(vec![Value::I32(WasiErrno::Success.as_u32() as i32)])
}

/// fd_prestat_get: Return preopen directory info for a given fd.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#fd_prestat_get>
fn wasi_fd_prestat_get(ctx: &WasiContext, args: Vec<Value>) -> Result<Vec<Value>, RuntimeError> {
    let fd = extract_i32(&args, 0)? as u32;
    let buf_ptr = extract_i32(&args, 1)? as u32;

    match ctx.get_preopen(fd) {
        Some(path) => {
            let path_len = path.len() as u32;
            // prestat struct: u8 tag (0 = dir) + 3 bytes padding + u32 name_len
            ctx.write_u32(buf_ptr, 0)?; // tag = __WASI_PREOPENTYPE_DIR (0)
            ctx.write_u32(buf_ptr + 4, path_len)?;
            Ok(vec![Value::I32(WasiErrno::Success.as_u32() as i32)])
        }
        None => Ok(vec![Value::I32(WasiErrno::BadF.as_u32() as i32)]),
    }
}

/// fd_prestat_dir_name: Return the path for a preopened fd.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#fd_prestat_dir_name>
fn wasi_fd_prestat_dir_name(ctx: &WasiContext, args: Vec<Value>) -> Result<Vec<Value>, RuntimeError> {
    let fd = extract_i32(&args, 0)? as u32;
    let path_ptr = extract_i32(&args, 1)? as u32;
    let path_len = extract_i32(&args, 2)? as u32;

    match ctx.get_preopen(fd) {
        Some(path) => {
            let bytes = path.as_bytes();
            if bytes.len() > path_len as usize {
                return Ok(vec![Value::I32(WasiErrno::Overflow.as_u32() as i32)]);
            }
            ctx.write_bytes(path_ptr, bytes)?;
            Ok(vec![Value::I32(WasiErrno::Success.as_u32() as i32)])
        }
        None => Ok(vec![Value::I32(WasiErrno::BadF.as_u32() as i32)]),
    }
}

/// path_open: Open a file or directory.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#path_open>
fn wasi_path_open(ctx: &WasiContext, args: Vec<Value>) -> Result<Vec<Value>, RuntimeError> {
    let dir_fd = extract_i32(&args, 0)? as u32;
    let _dirflags = extract_i32(&args, 1)?;
    let path_ptr = extract_i32(&args, 2)? as u32;
    let path_len = extract_i32(&args, 3)? as u32;
    let oflags = extract_i32(&args, 4)?;
    let _rights_base = extract_i64(&args, 5)?;
    let _rights_inheriting = extract_i64(&args, 6)?;
    let fdflags = extract_i32(&args, 7)?;
    let opened_fd_ptr = extract_i32(&args, 8)? as u32;

    // Read path from memory
    let path_bytes = ctx
        .read_bytes(path_ptr, path_len as usize)
        .map_err(|_| RuntimeError::MemoryError("Failed to read path".to_string()))?;
    let path =
        std::str::from_utf8(&path_bytes).map_err(|_| RuntimeError::MemoryError("Invalid UTF-8 path".to_string()))?;

    // Resolve path relative to preopen dir
    let resolved = match ctx.resolve_path(dir_fd, path) {
        Ok(p) => p,
        Err(errno) => return Ok(vec![Value::I32(errno.as_u32() as i32)]),
    };

    // Build open options from oflags and fdflags
    let o_creat = (oflags & 1) != 0;
    let o_excl = (oflags & 4) != 0;
    let o_trunc = (oflags & 8) != 0;
    let append = (fdflags & 1) != 0;

    let mut open_opts = std::fs::OpenOptions::new();

    // Determine read/write from rights (be permissive)
    open_opts.read(true);
    if o_creat || o_trunc || append {
        open_opts.write(true);
    }
    if o_creat {
        open_opts.create(true);
    }
    if o_excl {
        open_opts.create_new(true);
    }
    if o_trunc {
        open_opts.truncate(true);
    }
    if append {
        open_opts.append(true);
    }

    let file = match open_opts.open(&resolved) {
        Ok(f) => f,
        Err(e) => {
            let errno = match e.kind() {
                std::io::ErrorKind::NotFound => WasiErrno::NoEnt,
                std::io::ErrorKind::PermissionDenied => WasiErrno::Access,
                std::io::ErrorKind::AlreadyExists => WasiErrno::Exist,
                _ => WasiErrno::Io,
            };
            return Ok(vec![Value::I32(errno.as_u32() as i32)]);
        }
    };

    let writable = o_creat || o_trunc || append;
    let fd = match context::FileDescriptor::new_file(file, true, writable) {
        Ok(fd) => fd,
        Err(_) => return Ok(vec![Value::I32(WasiErrno::Io.as_u32() as i32)]),
    };

    let new_fd_num = ctx.allocate_fd(fd);
    ctx.write_u32(opened_fd_ptr, new_fd_num)?;

    Ok(vec![Value::I32(WasiErrno::Success.as_u32() as i32)])
}

/// fd_fdstat_get: Get the attributes of a file descriptor.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#fd_fdstat_get>
fn wasi_fd_fdstat_get(ctx: &WasiContext, args: Vec<Value>) -> Result<Vec<Value>, RuntimeError> {
    let fd = extract_i32(&args, 0)? as u32;
    let buf_ptr = extract_i32(&args, 1)? as u32;

    let fds = ctx.fds_ref();
    let fd_entry = match fds.get(fd as usize) {
        Some(Some(entry)) => entry,
        _ => return Ok(vec![Value::I32(WasiErrno::BadF.as_u32() as i32)]),
    };

    let filetype = fd_entry.file_type as u8;

    // fdstat struct: u8 filetype, u8 pad, u16 fdflags, u32 pad, u64 rights_base, u64 rights_inheriting
    // Total: 24 bytes
    let mut buf = [0u8; 24];
    buf[0] = filetype;
    // fdflags = 0, padding = 0
    // rights: be permissive, set all bits
    let all_rights: u64 = 0x1fff_ffff; // all defined rights
    buf[8..16].copy_from_slice(&all_rights.to_le_bytes());
    buf[16..24].copy_from_slice(&all_rights.to_le_bytes());

    drop(fds);
    ctx.write_bytes(buf_ptr, &buf)?;

    Ok(vec![Value::I32(WasiErrno::Success.as_u32() as i32)])
}

/// fd_seek: Move the offset of a file descriptor.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#fd_seek>
fn wasi_fd_seek(ctx: &WasiContext, args: Vec<Value>) -> Result<Vec<Value>, RuntimeError> {
    let fd = extract_i32(&args, 0)? as u32;
    let offset = extract_i64(&args, 1)?;
    let whence = extract_i32(&args, 2)? as u32;
    let newoffset_ptr = extract_i32(&args, 3)? as u32;

    match ctx.fd_seek(fd, offset, whence) {
        Ok(new_offset) => {
            ctx.write_u64(newoffset_ptr, new_offset)?;
            Ok(vec![Value::I32(WasiErrno::Success.as_u32() as i32)])
        }
        Err(errno) => Ok(vec![Value::I32(errno.as_u32() as i32)]),
    }
}

/// proc_exit: Terminate the process normally with the given exit code.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#proc_exit>
fn wasi_proc_exit(ctx: &WasiContext, args: Vec<Value>) -> Result<Vec<Value>, RuntimeError> {
    let code = extract_i32(&args, 0)?;

    ctx.set_exit_code(code);

    // Return a special error to signal exit
    Err(RuntimeError::Trap(format!("proc_exit({})", code)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::Memory;
    use std::sync::Mutex;

    #[test]
    fn test_create_wasi_imports() {
        let mut store = Store::new();
        let ctx = Arc::new(WasiContext::builder().build());

        let imports = create_wasi_imports(&mut store, ctx);

        // Check that all expected WASI functions are registered
        assert!(imports.get_function("wasi_snapshot_preview1", "fd_write").is_ok());
        assert!(imports.get_function("wasi_snapshot_preview1", "fd_read").is_ok());
        assert!(imports.get_function("wasi_snapshot_preview1", "fd_close").is_ok());
        assert!(imports.get_function("wasi_snapshot_preview1", "args_sizes_get").is_ok());
        assert!(imports.get_function("wasi_snapshot_preview1", "args_get").is_ok());
        assert!(
            imports
                .get_function("wasi_snapshot_preview1", "environ_sizes_get")
                .is_ok()
        );
        assert!(imports.get_function("wasi_snapshot_preview1", "environ_get").is_ok());
        assert!(imports.get_function("wasi_snapshot_preview1", "fd_prestat_get").is_ok());
        assert!(
            imports
                .get_function("wasi_snapshot_preview1", "fd_prestat_dir_name")
                .is_ok()
        );
        assert!(imports.get_function("wasi_snapshot_preview1", "path_open").is_ok());
        assert!(imports.get_function("wasi_snapshot_preview1", "fd_fdstat_get").is_ok());
        assert!(imports.get_function("wasi_snapshot_preview1", "fd_seek").is_ok());
        assert!(imports.get_function("wasi_snapshot_preview1", "proc_exit").is_ok());

        // env.abort should NOT be included in pure WASI imports
        assert!(imports.get_function("env", "abort").is_err());
    }

    #[test]
    fn test_create_wasi_imports_with_assemblyscript() {
        let mut store = Store::new();
        let ctx = Arc::new(WasiContext::builder().build());

        let mut imports = create_wasi_imports(&mut store, ctx.clone());
        add_assemblyscript_imports(&mut store, &mut imports, ctx);

        // Now env.abort should be available
        assert!(imports.get_function("env", "abort").is_ok());
    }

    #[test]
    fn test_args_sizes_get() {
        let ctx = WasiContext::builder().args(["prog", "hello", "world"]).build();

        // Bind memory
        let memory = Memory::new(1, None).unwrap();
        let shared_memory = Arc::new(Mutex::new(memory));
        ctx.bind_memory(shared_memory);

        let args = vec![Value::I32(0), Value::I32(4)]; // argc_ptr=0, argv_buf_size_ptr=4
        let result = wasi_args_sizes_get(&ctx, args).unwrap();

        assert_eq!(result, vec![Value::I32(0)]); // Success

        // Check argc = 3
        assert_eq!(ctx.read_u32(0).unwrap(), 3);
        // Check argv_buf_size = 5 + 6 + 6 = 17 ("prog\0" + "hello\0" + "world\0")
        assert_eq!(ctx.read_u32(4).unwrap(), 17);
    }

    #[test]
    fn test_args_get() {
        let ctx = WasiContext::builder().args(["prog", "hello"]).build();

        // Bind memory
        let memory = Memory::new(1, None).unwrap();
        let shared_memory = Arc::new(Mutex::new(memory));
        ctx.bind_memory(shared_memory);

        // argv_ptr = 0, argv_buf_ptr = 100
        let args = vec![Value::I32(0), Value::I32(100)];
        let result = wasi_args_get(&ctx, args).unwrap();

        assert_eq!(result, vec![Value::I32(0)]); // Success

        // Check argv[0] points to 100
        assert_eq!(ctx.read_u32(0).unwrap(), 100);
        // Check argv[1] points to 105 ("prog\0" is 5 bytes)
        assert_eq!(ctx.read_u32(4).unwrap(), 105);

        // Check strings
        let prog = ctx.read_bytes(100, 5).unwrap();
        assert_eq!(prog, b"prog\0");
        let hello = ctx.read_bytes(105, 6).unwrap();
        assert_eq!(hello, b"hello\0");
    }

    #[test]
    fn test_proc_exit() {
        let ctx = WasiContext::builder().build();

        let args = vec![Value::I32(42)];
        let result = wasi_proc_exit(&ctx, args);

        // Should return a trap error
        assert!(result.is_err());
        assert_eq!(ctx.exit_code(), Some(42));
    }
}
