//! WASI file descriptor operations
//!
//! fd_read, fd_write, fd_close, fd_seek, fd_tell, fd_fdstat_get,
//! fd_fdstat_set_flags, fd_filestat_get

use super::context::WasiContext;
use super::require_memory;
use super::types::{self, WasiErrno};
use crate::runtime::RuntimeError;
use crate::runtime::store::Caller;

/// fd_write: Write to a file descriptor using scatter/gather I/O.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#fd_write>
pub(super) fn wasi_fd_write<T>(
    ctx: &WasiContext,
    caller: &mut Caller<'_, T>,
    fd: i32,
    iovs_ptr: i32,
    iovs_len: i32,
    nwritten_ptr: i32,
) -> Result<i32, RuntimeError> {
    let memory = require_memory(caller)?;

    match ctx.fd_write(memory, fd as u32, iovs_ptr as u32, iovs_len as u32) {
        Ok(nwritten) => {
            memory.write_u32(nwritten_ptr as u32, nwritten as u32)?;
            Ok(WasiErrno::Success.as_i32())
        }
        Err(errno) => Ok(errno.as_i32()),
    }
}

/// fd_read: Read from a file descriptor using scatter/gather I/O.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#fd_read>
pub(super) fn wasi_fd_read<T>(
    ctx: &WasiContext,
    caller: &mut Caller<'_, T>,
    fd: i32,
    iovs_ptr: i32,
    iovs_len: i32,
    nread_ptr: i32,
) -> Result<i32, RuntimeError> {
    let memory = require_memory(caller)?;

    match ctx.fd_read(memory, fd as u32, iovs_ptr as u32, iovs_len as u32) {
        Ok(nread) => {
            memory.write_u32(nread_ptr as u32, nread as u32)?;
            Ok(WasiErrno::Success.as_i32())
        }
        Err(errno) => Ok(errno.as_i32()),
    }
}

/// fd_close: Close a file descriptor.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#fd_close>
pub(super) fn wasi_fd_close(ctx: &WasiContext, fd: i32) -> i32 {
    match ctx.close_fd(fd as u32) {
        Ok(()) => WasiErrno::Success.as_i32(),
        Err(errno) => errno.as_i32(),
    }
}

/// fd_seek: Move the offset of a file descriptor.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#fd_seek>
pub(super) fn wasi_fd_seek<T>(
    ctx: &WasiContext,
    caller: &mut Caller<'_, T>,
    fd: i32,
    offset: i64,
    whence: i32,
    newoffset_ptr: i32,
) -> Result<i32, RuntimeError> {
    match ctx.fd_seek(fd as u32, offset, whence as u32) {
        Ok(new_offset) => {
            let memory = require_memory(caller)?;
            memory.write_u64(newoffset_ptr as u32, new_offset)?;
            Ok(WasiErrno::Success.as_i32())
        }
        Err(errno) => Ok(errno.as_i32()),
    }
}

/// fd_tell: Return the current offset of a file descriptor.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#fd_tell>
pub(super) fn wasi_fd_tell<T>(
    ctx: &WasiContext,
    caller: &mut Caller<'_, T>,
    fd: i32,
    offset_ptr: i32,
) -> Result<i32, RuntimeError> {
    match ctx.fd_seek(fd as u32, 0, types::WHENCE_CUR) {
        Ok(offset) => {
            let memory = require_memory(caller)?;
            memory.write_u64(offset_ptr as u32, offset)?;
            Ok(WasiErrno::Success.as_i32())
        }
        Err(errno) => Ok(errno.as_i32()),
    }
}

/// fd_fdstat_get: Get the attributes of a file descriptor.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#fd_fdstat_get>
pub(super) fn wasi_fd_fdstat_get<T>(
    ctx: &WasiContext,
    caller: &mut Caller<'_, T>,
    fd: i32,
    buf_ptr: i32,
) -> Result<i32, RuntimeError> {
    let fds = ctx.fds_ref();
    let fd_entry = match fds.get(fd as usize) {
        Some(Some(entry)) => entry,
        _ => return Ok(WasiErrno::BadF.as_i32()),
    };

    let filetype = fd_entry.file_type as u8;

    // fdstat struct layout (24 bytes):
    //   0: fs_filetype (u8)
    //   2: fs_flags (u16, fdflags)
    //   8: fs_rights_base (u64)
    //  16: fs_rights_inheriting (u64)
    let mut buf = [0u8; 24];
    buf[0] = filetype;
    // fdflags at offset 2 = 0 (default)
    buf[8..16].copy_from_slice(&types::RIGHTS_ALL.to_le_bytes());
    buf[16..24].copy_from_slice(&types::RIGHTS_ALL.to_le_bytes());

    // Release the RefCell borrow before taking &mut Caller for memory access
    drop(fds);
    let memory = require_memory(caller)?;
    memory.write_bytes(buf_ptr as u32, &buf)?;

    Ok(WasiErrno::Success.as_i32())
}

/// fd_fdstat_set_flags: Adjust the flags associated with a file descriptor.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#fd_fdstat_set_flags>
pub(super) fn wasi_fd_fdstat_set_flags(ctx: &WasiContext, fd: i32, _flags: i32) -> i32 {
    let fds = ctx.fds_ref();
    match fds.get(fd as usize) {
        Some(Some(_)) => WasiErrno::Success.as_i32(),
        _ => WasiErrno::BadF.as_i32(),
    }
}

/// fd_filestat_get: Return the attributes of an open file.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#fd_filestat_get>
pub(super) fn wasi_fd_filestat_get<T>(
    ctx: &WasiContext,
    caller: &mut Caller<'_, T>,
    fd: i32,
    buf_ptr: i32,
) -> Result<i32, RuntimeError> {
    let fds = ctx.fds_ref();
    let fd_entry = match fds.get(fd as usize) {
        Some(Some(entry)) => entry,
        _ => return Ok(WasiErrno::BadF.as_i32()),
    };

    let filetype = fd_entry.file_type as u8;
    let buf = match &fd_entry.host_path {
        Some(path) => match std::fs::metadata(path) {
            Ok(meta) => write_filestat_buf(filetype, &meta),
            Err(e) => {
                drop(fds);
                return Ok(types::errno_from_io_error(&e).as_i32());
            }
        },
        None => write_filestat_buf_synthetic(filetype),
    };

    drop(fds);
    let memory = require_memory(caller)?;
    memory.write_bytes(buf_ptr as u32, &buf)?;
    Ok(WasiErrno::Success.as_i32())
}

/// Build a 64-byte filestat buffer from real filesystem metadata.
///
/// Struct layout: dev(8) + ino(8) + filetype(1+7pad) + nlink(8) +
/// size(8) + atim(8) + mtim(8) + ctim(8) = 64 bytes.
pub(super) fn write_filestat_buf(filetype: u8, meta: &std::fs::Metadata) -> [u8; 64] {
    let mut buf = [0u8; 64];

    // dev at offset 0 = 0
    // ino at offset 8 = 0
    buf[16] = filetype;
    // nlink at offset 24
    buf[24..32].copy_from_slice(&1u64.to_le_bytes());
    // size at offset 32
    buf[32..40].copy_from_slice(&meta.len().to_le_bytes());
    // Write timestamps as nanoseconds since UNIX epoch
    let time_nanos = |t: std::io::Result<std::time::SystemTime>| -> u64 {
        t.ok()
            .and_then(|t| t.duration_since(std::time::SystemTime::UNIX_EPOCH).ok())
            .map(|d| d.as_nanos().min(u64::MAX as u128) as u64)
            .unwrap_or(0)
    };
    // atim at offset 40
    buf[40..48].copy_from_slice(&time_nanos(meta.accessed()).to_le_bytes());
    // mtim at offset 48
    buf[48..56].copy_from_slice(&time_nanos(meta.modified()).to_le_bytes());
    // ctim at offset 56 (use modified — creation time not available on all platforms)
    buf[56..64].copy_from_slice(&time_nanos(meta.modified()).to_le_bytes());

    buf
}

/// Build a 64-byte filestat buffer with synthetic metadata (for stdin/stdout/stderr).
fn write_filestat_buf_synthetic(filetype: u8) -> [u8; 64] {
    let mut buf = [0u8; 64];
    buf[16] = filetype;
    buf[24..32].copy_from_slice(&1u64.to_le_bytes()); // nlink = 1
    buf
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::Memory;
    use crate::runtime::store::Caller;
    use crate::runtime::wasi::context::WasiContextBuilder;

    // Helper: set up a single iovec at offset 0 pointing to a buffer at `buf_offset`
    fn setup_iovec(memory: &mut Memory, buf_offset: u32, buf_len: u32) {
        memory.write_u32(0, buf_offset).unwrap(); // iov.buf
        memory.write_u32(4, buf_len).unwrap(); // iov.len
    }

    #[test]
    fn test_fd_write_stdout() {
        let output = std::sync::Arc::new(std::sync::Mutex::new(Vec::<u8>::new()));
        let writer = output.clone();
        let ctx = WasiContextBuilder::new().stdout(Box::new(SharedWriter(writer))).build();

        let mut memory = Memory::new(1, None).unwrap();
        // Write "hello" at offset 100
        memory.write_bytes(100, b"hello").unwrap();
        setup_iovec(&mut memory, 100, 5);

        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        // fd 1 = stdout, iovs at 0, 1 iov, nwritten at 200
        let result = wasi_fd_write(&ctx, &mut caller, 1, 0, 1, 200).unwrap();
        assert_eq!(result, 0);
        assert_eq!(memory.read_u32(200).unwrap(), 5);
        assert_eq!(&*output.lock().unwrap(), b"hello");
    }

    #[test]
    fn test_fd_write_badf() {
        let ctx = WasiContextBuilder::new().build();

        let mut memory = Memory::new(1, None).unwrap();
        setup_iovec(&mut memory, 100, 5);

        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        // fd 99 doesn't exist
        let result = wasi_fd_write(&ctx, &mut caller, 99, 0, 1, 200).unwrap();
        assert_eq!(result, WasiErrno::BadF.as_i32());
    }

    #[test]
    fn test_fd_read_stdin() {
        let ctx = WasiContextBuilder::new()
            .stdin(Box::new(std::io::Cursor::new(b"world".to_vec())))
            .build();

        let mut memory = Memory::new(1, None).unwrap();
        setup_iovec(&mut memory, 100, 64);

        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        // fd 0 = stdin, iovs at 0, 1 iov, nread at 200
        let result = wasi_fd_read(&ctx, &mut caller, 0, 0, 1, 200).unwrap();
        assert_eq!(result, 0);
        let nread = memory.read_u32(200).unwrap();
        assert_eq!(nread, 5);
        assert_eq!(memory.read_bytes(100, 5).unwrap(), b"world");
    }

    #[test]
    fn test_fd_read_eof() {
        let ctx = WasiContextBuilder::new()
            .stdin(Box::new(std::io::Cursor::new(Vec::<u8>::new())))
            .build();

        let mut memory = Memory::new(1, None).unwrap();
        setup_iovec(&mut memory, 100, 64);

        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_fd_read(&ctx, &mut caller, 0, 0, 1, 200).unwrap();
        assert_eq!(result, 0);
        assert_eq!(memory.read_u32(200).unwrap(), 0); // 0 bytes = EOF
    }

    #[test]
    fn test_fd_close() {
        let ctx = WasiContextBuilder::new().stdin(Box::new(std::io::empty())).build();

        // Close stdin (fd 0)
        let result = wasi_fd_close(&ctx, 0);
        assert_eq!(result, 0);

        // Closing again returns BADF
        let result = wasi_fd_close(&ctx, 0);
        assert_eq!(result, WasiErrno::BadF.as_i32());
    }

    #[test]
    fn test_fd_close_badf() {
        let ctx = WasiContextBuilder::new().build();
        let result = wasi_fd_close(&ctx, 99);
        assert_eq!(result, WasiErrno::BadF.as_i32());
    }

    #[test]
    fn test_fd_seek_and_tell() {
        let dir = std::env::temp_dir().join("krasm_test_fd_seek_tell");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("data.txt"), "0123456789").unwrap();

        let dir_str = dir.to_str().unwrap();
        let ctx = WasiContextBuilder::new().preopen_dir(dir_str, dir_str).build();

        // Open file via context
        let mut memory = Memory::new(1, None).unwrap();
        let mut data = ();

        // Use path_open to get a file fd -- easier to test seek/tell on a real file
        // by allocating directly via context
        let path = dir.join("data.txt");
        let file = std::fs::OpenOptions::new().read(true).open(&path).unwrap();
        let fd_entry = super::super::context::FileDescriptor::new_file(file, true, false, Some(path)).unwrap();
        let fd_num = ctx.allocate_fd(fd_entry).unwrap() as i32;

        // Seek to offset 5 from start (whence=0)
        {
            let mut caller = Caller::for_test(Some(&mut memory), &mut data);
            let result = wasi_fd_seek(&ctx, &mut caller, fd_num, 5, 0, 100).unwrap();
            assert_eq!(result, 0);
        }
        assert_eq!(memory.read_u64(100).unwrap(), 5);

        // fd_tell should report 5
        {
            let mut caller = Caller::for_test(Some(&mut memory), &mut data);
            let result = wasi_fd_tell(&ctx, &mut caller, fd_num, 200).unwrap();
            assert_eq!(result, 0);
        }
        assert_eq!(memory.read_u64(200).unwrap(), 5);

        // Seek 3 from current (whence=1)
        {
            let mut caller = Caller::for_test(Some(&mut memory), &mut data);
            let result = wasi_fd_seek(&ctx, &mut caller, fd_num, 3, 1, 100).unwrap();
            assert_eq!(result, 0);
        }
        assert_eq!(memory.read_u64(100).unwrap(), 8);

        // Seek to end (whence=2, offset=0)
        {
            let mut caller = Caller::for_test(Some(&mut memory), &mut data);
            let result = wasi_fd_seek(&ctx, &mut caller, fd_num, 0, 2, 100).unwrap();
            assert_eq!(result, 0);
        }
        assert_eq!(memory.read_u64(100).unwrap(), 10);

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_fd_fdstat_get_stdout() {
        let ctx = WasiContextBuilder::new().stdout(Box::new(std::io::sink())).build();

        let mut memory = Memory::new(1, None).unwrap();
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_fd_fdstat_get(&ctx, &mut caller, 1, 0).unwrap();
        assert_eq!(result, 0);

        // filetype at offset 0 = CharacterDevice (2)
        assert_eq!(memory.read_bytes(0, 1).unwrap()[0], 2);
    }

    #[test]
    fn test_fd_fdstat_get_directory() {
        let dir = std::env::temp_dir();
        let dir_str = dir.to_str().unwrap();
        let ctx = WasiContextBuilder::new().preopen_dir(dir_str, dir_str).build();

        let mut memory = Memory::new(1, None).unwrap();
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        // fd 3 = preopened directory
        let result = wasi_fd_fdstat_get(&ctx, &mut caller, 3, 0).unwrap();
        assert_eq!(result, 0);

        // filetype at offset 0 = Directory (3)
        assert_eq!(memory.read_bytes(0, 1).unwrap()[0], 3);
    }

    #[test]
    fn test_fd_fdstat_get_badf() {
        let ctx = WasiContextBuilder::new().build();

        let mut memory = Memory::new(1, None).unwrap();
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_fd_fdstat_get(&ctx, &mut caller, 99, 0).unwrap();
        assert_eq!(result, WasiErrno::BadF.as_i32());
    }

    #[test]
    fn test_fd_fdstat_set_flags() {
        let ctx = WasiContextBuilder::new().stdout(Box::new(std::io::sink())).build();

        // fd 1 exists -- should succeed
        let result = wasi_fd_fdstat_set_flags(&ctx, 1, 0);
        assert_eq!(result, 0);

        // fd 99 doesn't exist
        let result = wasi_fd_fdstat_set_flags(&ctx, 99, 0);
        assert_eq!(result, WasiErrno::BadF.as_i32());
    }

    #[test]
    fn test_fd_filestat_get_real_file() {
        let dir = std::env::temp_dir().join("krasm_test_fd_filestat");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("info.txt"), "twelve chars").unwrap();

        let path = dir.join("info.txt");
        let file = std::fs::OpenOptions::new().read(true).open(&path).unwrap();
        let fd_entry = super::super::context::FileDescriptor::new_file(file, true, false, Some(path)).unwrap();

        let ctx = WasiContextBuilder::new().build();
        let fd_num = ctx.allocate_fd(fd_entry).unwrap() as i32;

        let mut memory = Memory::new(1, None).unwrap();
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_fd_filestat_get(&ctx, &mut caller, fd_num, 0).unwrap();
        assert_eq!(result, 0);

        // filetype at offset 16 = RegularFile (4)
        assert_eq!(memory.read_bytes(16, 1).unwrap()[0], 4);
        // size at offset 32
        assert_eq!(memory.read_u64(32).unwrap(), 12);

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_fd_filestat_get_synthetic() {
        // stdin has no host_path, so gets synthetic metadata
        let ctx = WasiContextBuilder::new().stdin(Box::new(std::io::empty())).build();

        let mut memory = Memory::new(1, None).unwrap();
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_fd_filestat_get(&ctx, &mut caller, 0, 0).unwrap();
        assert_eq!(result, 0);

        // filetype at offset 16 = CharacterDevice (2)
        assert_eq!(memory.read_bytes(16, 1).unwrap()[0], 2);
        // size at offset 32 = 0 (synthetic)
        assert_eq!(memory.read_u64(32).unwrap(), 0);
    }

    /// A Write impl backed by a shared Vec for capturing output
    struct SharedWriter(std::sync::Arc<std::sync::Mutex<Vec<u8>>>);

    impl std::io::Write for SharedWriter {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.0.lock().unwrap().extend_from_slice(buf);
            Ok(buf.len())
        }
        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }
}
