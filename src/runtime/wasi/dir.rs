//! WASI directory and prestat operations
//!
//! fd_prestat_get, fd_prestat_dir_name, fd_readdir

use super::context::WasiContext;
use super::require_memory;
use super::types::{self, WasiErrno};
use crate::runtime::RuntimeError;
use crate::runtime::store::Caller;

/// fd_prestat_get: Return preopen directory info for a given fd.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#fd_prestat_get>
pub(super) fn wasi_fd_prestat_get<T>(
    ctx: &WasiContext,
    caller: &mut Caller<'_, T>,
    fd: i32,
    buf_ptr: i32,
) -> Result<i32, RuntimeError> {
    match ctx.get_preopen(fd as u32) {
        Some(path) => {
            let path_len = path.len() as u32;
            let memory = require_memory(caller)?;
            // prestat struct (8 bytes): u32 variant tag + u32 pr_name_len
            memory.write_u32(buf_ptr as u32, types::PREOPENTYPE_DIR)?;
            memory.write_u32(buf_ptr as u32 + 4, path_len)?;
            Ok(WasiErrno::Success.as_i32())
        }
        None => Ok(WasiErrno::BadF.as_i32()),
    }
}

/// fd_prestat_dir_name: Return the path for a preopened fd.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#fd_prestat_dir_name>
pub(super) fn wasi_fd_prestat_dir_name<T>(
    ctx: &WasiContext,
    caller: &mut Caller<'_, T>,
    fd: i32,
    path_ptr: i32,
    path_len: i32,
) -> Result<i32, RuntimeError> {
    match ctx.get_preopen(fd as u32) {
        Some(path) => {
            let bytes = path.as_bytes();
            if bytes.len() > path_len as usize {
                return Ok(WasiErrno::Overflow.as_i32());
            }
            let memory = require_memory(caller)?;
            memory.write_bytes(path_ptr as u32, bytes)?;
            Ok(WasiErrno::Success.as_i32())
        }
        None => Ok(WasiErrno::BadF.as_i32()),
    }
}

/// fd_readdir: Read directory entries from an open directory fd.
///
/// Each entry is a 24-byte dirent header followed by the entry name (NOT null-terminated).
/// Dirent layout: d_next(u64) + d_ino(u64) + d_namlen(u32) + d_type(u8) + 3pad.
/// Cookie 0 starts from the beginning. Entries are packed; if the buffer fills,
/// a partial result is returned.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#fd_readdir>
#[allow(clippy::too_many_arguments)]
pub(super) fn wasi_fd_readdir<T>(
    ctx: &WasiContext,
    caller: &mut Caller<'_, T>,
    fd: i32,
    buf_ptr: i32,
    buf_len: i32,
    cookie: i64,
    bufused_ptr: i32,
) -> Result<i32, RuntimeError> {
    let fds = ctx.fds_ref();
    let fd_entry = match fds.get(fd as usize) {
        Some(Some(entry)) => entry,
        _ => return Ok(WasiErrno::BadF.as_i32()),
    };

    let dir_path = match &fd_entry.host_path {
        Some(p) if fd_entry.file_type == types::WasiFileType::Directory => p.clone(),
        _ => return Ok(WasiErrno::NotDir.as_i32()),
    };
    drop(fds);

    // Read and sort entries for deterministic cookie ordering
    let mut entries: Vec<_> = match std::fs::read_dir(&dir_path) {
        Ok(iter) => iter.filter_map(|e| e.ok()).collect(),
        Err(e) => return Ok(types::errno_from_io_error(&e).as_i32()),
    };
    entries.sort_by_key(|e| e.file_name());

    let memory = require_memory(caller)?;
    let buf_start = buf_ptr as u32;
    let buf_capacity = buf_len as usize;
    let mut offset = 0usize;

    for (i, entry) in entries.iter().enumerate().skip(cookie as usize) {
        let name = entry.file_name();
        let name_bytes = name.as_encoded_bytes();
        let header_size = types::DIRENT_HEADER_SIZE as usize;

        // Need at least the header to include this entry
        if offset + header_size > buf_capacity {
            break;
        }

        // Build 24-byte dirent header
        let d_next = (i + 1) as u64;
        let d_namlen = name_bytes.len() as u32;
        let d_type = match entry.file_type() {
            Ok(ft) if ft.is_dir() => types::WasiFileType::Directory as u8,
            Ok(ft) if ft.is_symlink() => types::WasiFileType::SymbolicLink as u8,
            _ => types::WasiFileType::RegularFile as u8,
        };

        let mut header = [0u8; 24];
        header[0..8].copy_from_slice(&d_next.to_le_bytes());
        // d_ino at offset 8 = 0
        header[16..20].copy_from_slice(&d_namlen.to_le_bytes());
        header[20] = d_type;

        memory.write_bytes(buf_start + offset as u32, &header)?;
        offset += header_size;

        // Write as much of the name as fits in the remaining buffer
        let name_to_write = name_bytes.len().min(buf_capacity.saturating_sub(offset));
        if name_to_write > 0 {
            memory.write_bytes(buf_start + offset as u32, &name_bytes[..name_to_write])?;
            offset += name_to_write;
        }

        if offset >= buf_capacity {
            break;
        }
    }

    memory.write_u32(bufused_ptr as u32, offset as u32)?;
    Ok(WasiErrno::Success.as_i32())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::Memory;
    use crate::runtime::store::Caller;
    use crate::runtime::wasi::context::WasiContextBuilder;

    fn ctx_with_preopen(dir: &str) -> WasiContext {
        WasiContextBuilder::new().preopen_dir(dir, dir).build()
    }

    #[test]
    fn test_fd_prestat_get_success() {
        let dir = std::env::temp_dir();
        let dir_str = dir.to_str().unwrap();
        let ctx = ctx_with_preopen(dir_str);

        let mut memory = Memory::new(1, None).unwrap();
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_fd_prestat_get(&ctx, &mut caller, 3, 0).unwrap();
        assert_eq!(result, 0);

        // variant tag = PREOPENTYPE_DIR (0)
        assert_eq!(memory.read_u32(0).unwrap(), 0);
        // name length
        assert_eq!(memory.read_u32(4).unwrap(), dir_str.len() as u32);
    }

    #[test]
    fn test_fd_prestat_get_badf() {
        let ctx = WasiContextBuilder::new().build(); // no preopens

        let mut memory = Memory::new(1, None).unwrap();
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_fd_prestat_get(&ctx, &mut caller, 3, 0).unwrap();
        assert_eq!(result, WasiErrno::BadF.as_i32());
    }

    #[test]
    fn test_fd_prestat_dir_name() {
        let dir = std::env::temp_dir();
        let dir_str = dir.to_str().unwrap();
        let ctx = ctx_with_preopen(dir_str);

        let mut memory = Memory::new(1, None).unwrap();
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_fd_prestat_dir_name(&ctx, &mut caller, 3, 100, dir_str.len() as i32).unwrap();
        assert_eq!(result, 0);

        let bytes = memory.read_bytes(100, dir_str.len()).unwrap();
        assert_eq!(std::str::from_utf8(&bytes).unwrap(), dir_str);
    }

    #[test]
    fn test_fd_prestat_dir_name_overflow() {
        let dir = std::env::temp_dir();
        let dir_str = dir.to_str().unwrap();
        let ctx = ctx_with_preopen(dir_str);

        let mut memory = Memory::new(1, None).unwrap();
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        // Buffer too small
        let result = wasi_fd_prestat_dir_name(&ctx, &mut caller, 3, 100, 1).unwrap();
        assert_eq!(result, WasiErrno::Overflow.as_i32());
    }

    #[test]
    fn test_fd_readdir() {
        let dir = std::env::temp_dir().join("krasm_test_dir_readdir");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("aaa.txt"), "a").unwrap();
        std::fs::write(dir.join("bbb.txt"), "b").unwrap();
        std::fs::create_dir(dir.join("ccc")).unwrap();

        let dir_str = dir.to_str().unwrap();
        let ctx = ctx_with_preopen(dir_str);

        let mut memory = Memory::new(1, None).unwrap();
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        // fd 3 is the preopened directory
        let result = wasi_fd_readdir(&ctx, &mut caller, 3, 0, 4096, 0, 8000).unwrap();
        assert_eq!(result, 0);

        let bufused = memory.read_u32(8000).unwrap();
        assert!(bufused > 0, "readdir should return non-empty buffer");

        // Walk entries and collect names
        let mut names = Vec::new();
        let mut offset = 0u32;
        while offset + 24 <= bufused {
            let namelen = memory.read_u32(offset + 16).unwrap();
            if offset + 24 + namelen > bufused {
                break;
            }
            let name_bytes = memory.read_bytes(offset + 24, namelen as usize).unwrap();
            names.push(String::from_utf8(name_bytes).unwrap());
            offset += 24 + namelen;
        }

        // Sorted: aaa.txt, bbb.txt, ccc
        assert_eq!(names, vec!["aaa.txt", "bbb.txt", "ccc"]);

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_fd_readdir_with_cookie() {
        let dir = std::env::temp_dir().join("krasm_test_dir_readdir_cookie");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("alpha"), "").unwrap();
        std::fs::write(dir.join("beta"), "").unwrap();
        std::fs::write(dir.join("gamma"), "").unwrap();

        let dir_str = dir.to_str().unwrap();
        let ctx = ctx_with_preopen(dir_str);

        let mut memory = Memory::new(1, None).unwrap();
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        // Skip first 2 entries (cookie=2), should get only "gamma"
        let result = wasi_fd_readdir(&ctx, &mut caller, 3, 0, 4096, 2, 8000).unwrap();
        assert_eq!(result, 0);

        let bufused = memory.read_u32(8000).unwrap();
        let mut names = Vec::new();
        let mut offset = 0u32;
        while offset + 24 <= bufused {
            let namelen = memory.read_u32(offset + 16).unwrap();
            if offset + 24 + namelen > bufused {
                break;
            }
            let name_bytes = memory.read_bytes(offset + 24, namelen as usize).unwrap();
            names.push(String::from_utf8(name_bytes).unwrap());
            offset += 24 + namelen;
        }

        assert_eq!(names, vec!["gamma"]);

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_fd_readdir_not_a_directory() {
        // fd 0 (stdin) is not a directory
        let ctx = WasiContextBuilder::new().stdin(Box::new(std::io::empty())).build();

        let mut memory = Memory::new(1, None).unwrap();
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_fd_readdir(&ctx, &mut caller, 0, 0, 4096, 0, 8000).unwrap();
        assert_eq!(result, WasiErrno::NotDir.as_i32());
    }

    #[test]
    fn test_fd_readdir_small_buffer() {
        let dir = std::env::temp_dir().join("krasm_test_dir_readdir_small");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("file1.txt"), "").unwrap();
        std::fs::write(dir.join("file2.txt"), "").unwrap();

        let dir_str = dir.to_str().unwrap();
        let ctx = ctx_with_preopen(dir_str);

        let mut memory = Memory::new(1, None).unwrap();
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        // Buffer only big enough for one entry (24-byte header + ~9 name bytes)
        let result = wasi_fd_readdir(&ctx, &mut caller, 3, 0, 34, 0, 8000).unwrap();
        assert_eq!(result, 0);

        let bufused = memory.read_u32(8000).unwrap();
        // Should have exactly one entry: 24 header + 9 name = 33 bytes
        assert_eq!(bufused, 33);

        let _ = std::fs::remove_dir_all(&dir);
    }
}
