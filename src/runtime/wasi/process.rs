//! WASI process and environment operations
//!
//! args_sizes_get, args_get, environ_sizes_get, environ_get, proc_exit

use super::context::WasiContext;
use super::require_memory;
use super::types::WasiErrno;
use crate::runtime::RuntimeError;
use crate::runtime::store::Caller;

/// args_sizes_get: Return the number of arguments and total buffer size.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#args_sizes_get>
pub(super) fn wasi_args_sizes_get<T>(
    ctx: &WasiContext,
    caller: &mut Caller<'_, T>,
    argc_ptr: i32,
    argv_buf_size_ptr: i32,
) -> Result<i32, RuntimeError> {
    let memory = require_memory(caller)?;

    let argc = ctx.args().len() as u32;
    let buf_size: u32 = ctx.args().iter().map(|a| a.len() as u32 + 1).sum();

    memory.write_u32(argc_ptr as u32, argc)?;
    memory.write_u32(argv_buf_size_ptr as u32, buf_size)?;

    Ok(WasiErrno::Success.as_i32())
}

/// args_get: Write argument pointers and string data to memory.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#args_get>
pub(super) fn wasi_args_get<T>(
    ctx: &WasiContext,
    caller: &mut Caller<'_, T>,
    argv_ptr: i32,
    argv_buf_ptr: i32,
) -> Result<i32, RuntimeError> {
    let memory = require_memory(caller)?;

    let mut buf_offset = 0u32;
    for (i, arg) in ctx.args().iter().enumerate() {
        let arg_addr = (argv_buf_ptr as u32)
            .checked_add(buf_offset)
            .ok_or_else(|| RuntimeError::MemoryError("argv buffer pointer overflow".to_string()))?;
        let ptr_offset = (i as u32)
            .checked_mul(4)
            .and_then(|x| (argv_ptr as u32).checked_add(x))
            .ok_or_else(|| RuntimeError::MemoryError("argv pointer overflow".to_string()))?;
        memory.write_u32(ptr_offset, arg_addr)?;

        let mut arg_bytes = arg.as_bytes().to_vec();
        arg_bytes.push(0); // null terminator
        memory.write_bytes(arg_addr, &arg_bytes)?;

        buf_offset += arg_bytes.len() as u32;
    }

    Ok(WasiErrno::Success.as_i32())
}

/// environ_sizes_get: Return the number of environment variables and total buffer size.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#environ_sizes_get>
pub(super) fn wasi_environ_sizes_get<T>(
    ctx: &WasiContext,
    caller: &mut Caller<'_, T>,
    environc_ptr: i32,
    environ_buf_size_ptr: i32,
) -> Result<i32, RuntimeError> {
    let memory = require_memory(caller)?;

    let environc = ctx.env().len() as u32;
    let buf_size: u32 = ctx.env().iter().map(|e| e.len() as u32 + 1).sum();

    memory.write_u32(environc_ptr as u32, environc)?;
    memory.write_u32(environ_buf_size_ptr as u32, buf_size)?;

    Ok(WasiErrno::Success.as_i32())
}

/// environ_get: Write environment variable pointers and string data to memory.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#environ_get>
pub(super) fn wasi_environ_get<T>(
    ctx: &WasiContext,
    caller: &mut Caller<'_, T>,
    environ_ptr: i32,
    environ_buf_ptr: i32,
) -> Result<i32, RuntimeError> {
    let memory = require_memory(caller)?;

    let mut buf_offset = 0u32;
    for (i, env_var) in ctx.env().iter().enumerate() {
        let env_addr = (environ_buf_ptr as u32)
            .checked_add(buf_offset)
            .ok_or_else(|| RuntimeError::MemoryError("environ buffer pointer overflow".to_string()))?;
        let ptr_offset = (i as u32)
            .checked_mul(4)
            .and_then(|x| (environ_ptr as u32).checked_add(x))
            .ok_or_else(|| RuntimeError::MemoryError("environ pointer overflow".to_string()))?;
        memory.write_u32(ptr_offset, env_addr)?;

        let mut env_bytes = env_var.as_bytes().to_vec();
        env_bytes.push(0);
        memory.write_bytes(env_addr, &env_bytes)?;

        buf_offset += env_bytes.len() as u32;
    }

    Ok(WasiErrno::Success.as_i32())
}

/// proc_exit: Terminate the process.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#proc_exit>
pub(super) fn wasi_proc_exit(ctx: &WasiContext, rval: i32) -> Result<(), RuntimeError> {
    ctx.set_exit_code(rval);
    Err(RuntimeError::Trap(format!("proc_exit({})", rval)))
}
