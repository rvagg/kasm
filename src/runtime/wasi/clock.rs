//! WASI clock, random, and scheduling operations
//!
//! clock_time_get, clock_res_get, random_get, sched_yield

use super::context::WasiContext;
use super::require_memory;
use super::types::{self, WasiErrno};
use crate::runtime::RuntimeError;
use crate::runtime::store::Caller;
use std::time::{Instant, SystemTime};

/// clock_time_get: Return the time value of a clock.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#clock_time_get>
pub(super) fn wasi_clock_time_get<T>(
    ctx: &WasiContext,
    caller: &mut Caller<'_, T>,
    clockid: i32,
    _precision: i64,
    time_ptr: i32,
) -> Result<i32, RuntimeError> {
    let nanos = match clockid as u32 {
        types::CLOCKID_REALTIME => {
            let duration = SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap_or_default();
            duration.as_nanos() as u64
        }
        types::CLOCKID_MONOTONIC => {
            let elapsed = Instant::now().duration_since(ctx.monotonic_epoch());
            elapsed.as_nanos() as u64
        }
        _ => return Ok(WasiErrno::NoSys.as_i32()),
    };

    let memory = require_memory(caller)?;
    memory.write_u64(time_ptr as u32, nanos)?;
    Ok(WasiErrno::Success.as_i32())
}

/// clock_res_get: Return the resolution of a clock.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#clock_res_get>
pub(super) fn wasi_clock_res_get<T>(
    caller: &mut Caller<'_, T>,
    clockid: i32,
    resolution_ptr: i32,
) -> Result<i32, RuntimeError> {
    let res_ns: u64 = match clockid as u32 {
        types::CLOCKID_REALTIME | types::CLOCKID_MONOTONIC => 1,
        _ => return Ok(WasiErrno::NoSys.as_i32()),
    };

    let memory = require_memory(caller)?;
    memory.write_u64(resolution_ptr as u32, res_ns)?;
    Ok(WasiErrno::Success.as_i32())
}

/// random_get: Write high-quality random data into a buffer.
///
/// Uses the `rand` crate (backed by `getrandom`) for cross-platform
/// cryptographic randomness.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#random_get>
pub(super) fn wasi_random_get<T>(caller: &mut Caller<'_, T>, buf_ptr: i32, buf_len: i32) -> Result<i32, RuntimeError> {
    if buf_len < 0 {
        return Ok(WasiErrno::Inval.as_i32());
    }
    let len = buf_len as usize;
    let mut buf = vec![0u8; len];
    rand::fill(&mut buf[..]);

    let memory = require_memory(caller)?;
    memory.write_bytes(buf_ptr as u32, &buf)?;
    Ok(WasiErrno::Success.as_i32())
}

/// sched_yield: Temporarily yield execution of the calling thread.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#sched_yield>
pub(super) fn wasi_sched_yield() -> i32 {
    std::thread::yield_now();
    WasiErrno::Success.as_i32()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::Memory;
    use crate::runtime::store::Caller;

    #[test]
    fn test_clock_time_get_realtime() {
        let ctx = WasiContext::builder().build();
        let mut memory = Memory::new(1, None).unwrap();
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_clock_time_get(&ctx, &mut caller, types::CLOCKID_REALTIME as i32, 1000, 0).unwrap();
        assert_eq!(result, 0);

        let nanos = memory.read_u64(0).unwrap();
        // Sanity: should be after 2020-01-01 (1577836800 seconds)
        assert!(nanos > 1_577_836_800_000_000_000);
    }

    #[test]
    fn test_clock_time_get_monotonic() {
        let ctx = WasiContext::builder().build();
        // Small sleep so elapsed time is measurably non-zero
        std::thread::sleep(std::time::Duration::from_millis(1));

        let mut memory = Memory::new(1, None).unwrap();
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_clock_time_get(&ctx, &mut caller, types::CLOCKID_MONOTONIC as i32, 1000, 0).unwrap();
        assert_eq!(result, 0);

        let nanos = memory.read_u64(0).unwrap();
        assert!(nanos > 0, "Monotonic clock should report non-zero elapsed time");
        // Should be small (well under 10 seconds)
        assert!(nanos < 10_000_000_000, "Monotonic time unexpectedly large: {nanos}");
    }

    #[test]
    fn test_clock_time_get_unsupported() {
        let ctx = WasiContext::builder().build();
        let mut memory = Memory::new(1, None).unwrap();
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_clock_time_get(&ctx, &mut caller, types::CLOCKID_PROCESS_CPUTIME as i32, 0, 0).unwrap();
        assert_eq!(result, WasiErrno::NoSys.as_i32());
    }

    #[test]
    fn test_clock_res_get_realtime() {
        let mut memory = Memory::new(1, None).unwrap();
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_clock_res_get(&mut caller, types::CLOCKID_REALTIME as i32, 0).unwrap();
        assert_eq!(result, 0);

        let res = memory.read_u64(0).unwrap();
        assert_eq!(res, 1);
    }

    #[test]
    fn test_clock_res_get_unsupported() {
        let mut memory = Memory::new(1, None).unwrap();
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_clock_res_get(&mut caller, types::CLOCKID_THREAD_CPUTIME as i32, 0).unwrap();
        assert_eq!(result, WasiErrno::NoSys.as_i32());
    }

    #[test]
    fn test_random_get() {
        let mut memory = Memory::new(1, None).unwrap();
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_random_get(&mut caller, 0, 32).unwrap();
        assert_eq!(result, 0);

        let bytes = memory.read_bytes(0, 32).unwrap();
        // Extremely unlikely that 32 random bytes are all zero
        assert!(bytes.iter().any(|&b| b != 0), "random bytes should not all be zero");
    }

    #[test]
    fn test_sched_yield() {
        let result = wasi_sched_yield();
        assert_eq!(result, 0);
    }
}
