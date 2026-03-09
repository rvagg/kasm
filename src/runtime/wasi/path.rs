//! WASI path operations
//!
//! path_open, path_filestat_get, path_create_directory, path_remove_directory,
//! path_unlink_file, path_rename

use super::context::{self, WasiContext};
use super::require_memory;
use super::types::{self, WasiErrno};
use crate::runtime::RuntimeError;
use crate::runtime::store::Caller;

/// path_open: Open a file or directory.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#path_open>
#[allow(clippy::too_many_arguments)]
pub(super) fn wasi_path_open<T>(
    ctx: &WasiContext,
    caller: &mut Caller<'_, T>,
    dir_fd: i32,
    dirflags: i32,
    path_ptr: i32,
    path_len: i32,
    oflags: i32,
    _rights_base: i64,
    _rights_inheriting: i64,
    fdflags: i32,
    opened_fd_ptr: i32,
) -> Result<i32, RuntimeError> {
    // dirflags: LOOKUP_SYMLINK_FOLLOW controls whether the final path
    // component follows symlinks. resolve_path() canonicalises the path
    // which implicitly follows all symlinks. When the flag is absent,
    // the spec says symlinks at the final component should NOT be followed.
    //
    // TODO: implement no-follow mode. This requires using lstat() on the
    // final component and returning ELOOP if it's a symlink. Currently we
    // always follow, which is the permissive (less secure) behaviour.
    let _symlink_follow = (dirflags as u32 & types::LOOKUP_SYMLINK_FOLLOW) != 0;

    if path_ptr < 0 || path_len < 0 {
        return Ok(WasiErrno::Inval.as_i32());
    }

    let memory = require_memory(caller)?;

    // Read path from memory
    let path_bytes = memory
        .read_bytes(path_ptr as u32, path_len as usize)
        .map_err(|_| RuntimeError::MemoryError("failed to read path".to_string()))?;
    let path =
        std::str::from_utf8(&path_bytes).map_err(|_| RuntimeError::MemoryError("invalid utf-8 path".to_string()))?;

    // Resolve path relative to preopen dir
    let resolved = match ctx.resolve_path(dir_fd as u32, path) {
        Ok(p) => p,
        Err(errno) => return Ok(errno.as_i32()),
    };

    // Build open options from oflags and fdflags
    let oflags = oflags as u16;
    let fdflags = fdflags as u16;
    let o_creat = (oflags & types::OFLAG_CREAT) != 0;
    let o_directory = (oflags & types::OFLAG_DIRECTORY) != 0;
    let o_excl = (oflags & types::OFLAG_EXCL) != 0;
    let o_trunc = (oflags & types::OFLAG_TRUNC) != 0;
    let append = (fdflags & types::FDFLAG_APPEND) != 0;

    // Handle directory opens: return a directory fd for readdir/stat
    if o_directory || resolved.is_dir() {
        if !resolved.is_dir() {
            return Ok(WasiErrno::NotDir.as_i32());
        }
        let fd = context::FileDescriptor::new_directory(resolved);
        let new_fd_num = match ctx.allocate_fd(fd) {
            Ok(n) => n,
            Err(errno) => return Ok(errno.as_i32()),
        };
        memory.write_u32(opened_fd_ptr as u32, new_fd_num)?;
        return Ok(WasiErrno::Success.as_i32());
    }

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
            return Ok(types::errno_from_io_error(&e).as_i32());
        }
    };

    let writable = o_creat || o_trunc || append;
    let fd = match context::FileDescriptor::new_file(file, true, writable, Some(resolved.clone())) {
        Ok(fd) => fd,
        Err(_) => return Ok(WasiErrno::Io.as_i32()),
    };

    let new_fd_num = match ctx.allocate_fd(fd) {
        Ok(n) => n,
        Err(errno) => return Ok(errno.as_i32()),
    };
    memory.write_u32(opened_fd_ptr as u32, new_fd_num)?;

    Ok(WasiErrno::Success.as_i32())
}

/// path_filestat_get: Return the attributes of a file or directory.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#path_filestat_get>
pub(super) fn wasi_path_filestat_get<T>(
    ctx: &WasiContext,
    caller: &mut Caller<'_, T>,
    dir_fd: i32,
    flags: i32,
    path_ptr: i32,
    path_len: i32,
    buf_ptr: i32,
) -> Result<i32, RuntimeError> {
    if path_ptr < 0 || path_len < 0 {
        return Ok(WasiErrno::Inval.as_i32());
    }

    let memory = require_memory(caller)?;

    let path_bytes = memory
        .read_bytes(path_ptr as u32, path_len as usize)
        .map_err(|_| RuntimeError::MemoryError("failed to read path".to_string()))?;
    let path =
        std::str::from_utf8(&path_bytes).map_err(|_| RuntimeError::MemoryError("invalid utf-8 path".to_string()))?;

    let resolved = match ctx.resolve_path(dir_fd as u32, path) {
        Ok(p) => p,
        Err(errno) => return Ok(errno.as_i32()),
    };

    let symlink_follow = (flags as u32 & types::LOOKUP_SYMLINK_FOLLOW) != 0;
    let meta = if symlink_follow {
        std::fs::metadata(&resolved)
    } else {
        std::fs::symlink_metadata(&resolved)
    };

    match meta {
        Ok(m) => {
            let filetype = if m.is_dir() {
                types::WasiFileType::Directory as u8
            } else if m.is_symlink() {
                types::WasiFileType::SymbolicLink as u8
            } else {
                types::WasiFileType::RegularFile as u8
            };
            let buf = super::fd::write_filestat_buf(filetype, &m);
            memory.write_bytes(buf_ptr as u32, &buf)?;
            Ok(WasiErrno::Success.as_i32())
        }
        Err(e) => Ok(types::errno_from_io_error(&e).as_i32()),
    }
}

/// Read a UTF-8 path from guest memory.
///
/// Validates that path_ptr and path_len are non-negative before casting to
/// unsigned types, preventing confusing OOM on negative values.
fn read_guest_path<T>(caller: &mut Caller<'_, T>, path_ptr: i32, path_len: i32) -> Result<String, RuntimeError> {
    if path_ptr < 0 || path_len < 0 {
        return Err(RuntimeError::MemoryError("invalid path pointer or length".to_string()));
    }
    let memory = require_memory(caller)?;
    let bytes = memory
        .read_bytes(path_ptr as u32, path_len as usize)
        .map_err(|_| RuntimeError::MemoryError("failed to read path".to_string()))?;
    String::from_utf8(bytes).map_err(|_| RuntimeError::MemoryError("invalid utf-8 path".to_string()))
}

/// path_create_directory: Create a directory.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#path_create_directory>
pub(super) fn wasi_path_create_directory<T>(
    ctx: &WasiContext,
    caller: &mut Caller<'_, T>,
    fd: i32,
    path_ptr: i32,
    path_len: i32,
) -> Result<i32, RuntimeError> {
    let path = read_guest_path(caller, path_ptr, path_len)?;
    let resolved = match ctx.resolve_path(fd as u32, &path) {
        Ok(p) => p,
        Err(errno) => return Ok(errno.as_i32()),
    };
    match std::fs::create_dir(&resolved) {
        Ok(()) => Ok(WasiErrno::Success.as_i32()),
        Err(e) => Ok(types::errno_from_io_error(&e).as_i32()),
    }
}

/// path_remove_directory: Remove a directory.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#path_remove_directory>
pub(super) fn wasi_path_remove_directory<T>(
    ctx: &WasiContext,
    caller: &mut Caller<'_, T>,
    fd: i32,
    path_ptr: i32,
    path_len: i32,
) -> Result<i32, RuntimeError> {
    let path = read_guest_path(caller, path_ptr, path_len)?;
    let resolved = match ctx.resolve_path(fd as u32, &path) {
        Ok(p) => p,
        Err(errno) => return Ok(errno.as_i32()),
    };
    match std::fs::remove_dir(&resolved) {
        Ok(()) => Ok(WasiErrno::Success.as_i32()),
        Err(e) => Ok(types::errno_from_io_error(&e).as_i32()),
    }
}

/// path_unlink_file: Unlink a file.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#path_unlink_file>
pub(super) fn wasi_path_unlink_file<T>(
    ctx: &WasiContext,
    caller: &mut Caller<'_, T>,
    fd: i32,
    path_ptr: i32,
    path_len: i32,
) -> Result<i32, RuntimeError> {
    let path = read_guest_path(caller, path_ptr, path_len)?;
    let resolved = match ctx.resolve_path(fd as u32, &path) {
        Ok(p) => p,
        Err(errno) => return Ok(errno.as_i32()),
    };
    match std::fs::remove_file(&resolved) {
        Ok(()) => Ok(WasiErrno::Success.as_i32()),
        Err(e) => Ok(types::errno_from_io_error(&e).as_i32()),
    }
}

/// path_rename: Rename a file or directory.
///
/// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#path_rename>
#[allow(clippy::too_many_arguments)]
pub(super) fn wasi_path_rename<T>(
    ctx: &WasiContext,
    caller: &mut Caller<'_, T>,
    old_fd: i32,
    old_path_ptr: i32,
    old_path_len: i32,
    new_fd: i32,
    new_path_ptr: i32,
    new_path_len: i32,
) -> Result<i32, RuntimeError> {
    let old_path = read_guest_path(caller, old_path_ptr, old_path_len)?;
    let new_path = read_guest_path(caller, new_path_ptr, new_path_len)?;

    let old_resolved = match ctx.resolve_path(old_fd as u32, &old_path) {
        Ok(p) => p,
        Err(errno) => return Ok(errno.as_i32()),
    };
    let new_resolved = match ctx.resolve_path(new_fd as u32, &new_path) {
        Ok(p) => p,
        Err(errno) => return Ok(errno.as_i32()),
    };

    match std::fs::rename(&old_resolved, &new_resolved) {
        Ok(()) => Ok(WasiErrno::Success.as_i32()),
        Err(e) => Ok(types::errno_from_io_error(&e).as_i32()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::Memory;
    use crate::runtime::store::Caller;
    use crate::runtime::wasi::context::WasiContextBuilder;

    /// Build a WasiContext with a single preopen at fd 3.
    fn ctx_with_preopen(dir: &std::path::Path) -> WasiContext {
        let s = dir.to_str().unwrap();
        WasiContextBuilder::new().preopen_dir(s, s).build()
    }

    /// Write a path string into guest memory at the given offset
    /// and return (ptr, len) suitable for passing to WASI functions.
    fn write_path(memory: &mut Memory, offset: u32, path: &str) -> (i32, i32) {
        memory.write_bytes(offset, path.as_bytes()).unwrap();
        (offset as i32, path.len() as i32)
    }

    // -- path_open --

    #[test]
    fn test_path_open_read_existing_file() {
        let dir = std::env::temp_dir().join("kasm_test_path_open_read");
        let _ = std::fs::create_dir_all(&dir);
        std::fs::write(dir.join("hello.txt"), b"hello").unwrap();

        let ctx = ctx_with_preopen(&dir);
        let mut memory = Memory::new(1, None).unwrap();
        let (pp, pl) = write_path(&mut memory, 0, "hello.txt");
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_path_open(&ctx, &mut caller, 3, 0, pp, pl, 0, 0, 0, 0, 256).unwrap();
        assert_eq!(result, WasiErrno::Success.as_i32());

        let opened_fd = memory.read_u32(256).unwrap();
        assert!(opened_fd >= 4, "opened fd should be >= 4, got {opened_fd}");

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_path_open_create_file() {
        let dir = std::env::temp_dir().join("kasm_test_path_open_create");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();

        let ctx = ctx_with_preopen(&dir);
        let mut memory = Memory::new(1, None).unwrap();
        let (pp, pl) = write_path(&mut memory, 0, "new_file.txt");
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let oflags = types::OFLAG_CREAT as i32;
        let result = wasi_path_open(&ctx, &mut caller, 3, 0, pp, pl, oflags, 0, 0, 0, 256).unwrap();
        assert_eq!(result, WasiErrno::Success.as_i32());
        assert!(dir.join("new_file.txt").exists());

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_path_open_excl_existing_fails() {
        let dir = std::env::temp_dir().join("kasm_test_path_open_excl");
        let _ = std::fs::create_dir_all(&dir);
        std::fs::write(dir.join("exists.txt"), b"data").unwrap();

        let ctx = ctx_with_preopen(&dir);
        let mut memory = Memory::new(1, None).unwrap();
        let (pp, pl) = write_path(&mut memory, 0, "exists.txt");
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let oflags = (types::OFLAG_CREAT | types::OFLAG_EXCL) as i32;
        let result = wasi_path_open(&ctx, &mut caller, 3, 0, pp, pl, oflags, 0, 0, 0, 256).unwrap();
        assert_ne!(
            result,
            WasiErrno::Success.as_i32(),
            "O_EXCL on existing file should fail"
        );

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_path_open_directory() {
        let dir = std::env::temp_dir().join("kasm_test_path_open_dir");
        let _ = std::fs::create_dir_all(dir.join("subdir"));

        let ctx = ctx_with_preopen(&dir);
        let mut memory = Memory::new(1, None).unwrap();
        let (pp, pl) = write_path(&mut memory, 0, "subdir");
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let oflags = types::OFLAG_DIRECTORY as i32;
        let result = wasi_path_open(&ctx, &mut caller, 3, 0, pp, pl, oflags, 0, 0, 0, 256).unwrap();
        assert_eq!(result, WasiErrno::Success.as_i32());

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_path_open_directory_on_file_fails() {
        let dir = std::env::temp_dir().join("kasm_test_path_open_dir_file");
        let _ = std::fs::create_dir_all(&dir);
        std::fs::write(dir.join("file.txt"), b"data").unwrap();

        let ctx = ctx_with_preopen(&dir);
        let mut memory = Memory::new(1, None).unwrap();
        let (pp, pl) = write_path(&mut memory, 0, "file.txt");
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let oflags = types::OFLAG_DIRECTORY as i32;
        let result = wasi_path_open(&ctx, &mut caller, 3, 0, pp, pl, oflags, 0, 0, 0, 256).unwrap();
        assert_eq!(result, WasiErrno::NotDir.as_i32());

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_path_open_nonexistent() {
        let dir = std::env::temp_dir().join("kasm_test_path_open_noent");
        let _ = std::fs::create_dir_all(&dir);

        let ctx = ctx_with_preopen(&dir);
        let mut memory = Memory::new(1, None).unwrap();
        let (pp, pl) = write_path(&mut memory, 0, "no_such_file.txt");
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_path_open(&ctx, &mut caller, 3, 0, pp, pl, 0, 0, 0, 0, 256).unwrap();
        assert_ne!(result, WasiErrno::Success.as_i32());

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_path_open_negative_ptr() {
        let dir = std::env::temp_dir().join("kasm_test_path_open_neg");
        let _ = std::fs::create_dir_all(&dir);

        let ctx = ctx_with_preopen(&dir);
        let mut memory = Memory::new(1, None).unwrap();
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_path_open(&ctx, &mut caller, 3, 0, -1, 5, 0, 0, 0, 0, 256).unwrap();
        assert_eq!(result, WasiErrno::Inval.as_i32());

        let _ = std::fs::remove_dir_all(&dir);
    }

    // -- path_filestat_get --

    #[test]
    fn test_path_filestat_get_file() {
        let dir = std::env::temp_dir().join("kasm_test_path_fstat_file");
        let _ = std::fs::create_dir_all(&dir);
        std::fs::write(dir.join("data.bin"), b"abcdef").unwrap();

        let ctx = ctx_with_preopen(&dir);
        let mut memory = Memory::new(1, None).unwrap();
        let (pp, pl) = write_path(&mut memory, 0, "data.bin");
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let flags = types::LOOKUP_SYMLINK_FOLLOW as i32;
        let result = wasi_path_filestat_get(&ctx, &mut caller, 3, flags, pp, pl, 256).unwrap();
        assert_eq!(result, WasiErrno::Success.as_i32());

        // filestat layout: dev(8) ino(8) filetype(1+pad to 8) nlink(8) size(8) ...
        // filetype at offset 16
        let filetype = memory.read_bytes(256 + 16, 1).unwrap()[0];
        assert_eq!(filetype, types::WasiFileType::RegularFile as u8);

        // size at offset 32
        let size = memory.read_u64(256 + 32).unwrap();
        assert_eq!(size, 6);

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_path_filestat_get_directory() {
        let dir = std::env::temp_dir().join("kasm_test_path_fstat_dir");
        let _ = std::fs::create_dir_all(dir.join("child"));

        let ctx = ctx_with_preopen(&dir);
        let mut memory = Memory::new(1, None).unwrap();
        let (pp, pl) = write_path(&mut memory, 0, "child");
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_path_filestat_get(&ctx, &mut caller, 3, 0, pp, pl, 256).unwrap();
        assert_eq!(result, WasiErrno::Success.as_i32());

        let filetype = memory.read_bytes(256 + 16, 1).unwrap()[0];
        assert_eq!(filetype, types::WasiFileType::Directory as u8);

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_path_filestat_get_nonexistent() {
        let dir = std::env::temp_dir().join("kasm_test_path_fstat_noent");
        let _ = std::fs::create_dir_all(&dir);

        let ctx = ctx_with_preopen(&dir);
        let mut memory = Memory::new(1, None).unwrap();
        let (pp, pl) = write_path(&mut memory, 0, "ghost.txt");
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_path_filestat_get(&ctx, &mut caller, 3, 0, pp, pl, 256).unwrap();
        assert_ne!(result, WasiErrno::Success.as_i32());

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_path_filestat_get_negative_len() {
        let dir = std::env::temp_dir().join("kasm_test_path_fstat_neg");
        let _ = std::fs::create_dir_all(&dir);

        let ctx = ctx_with_preopen(&dir);
        let mut memory = Memory::new(1, None).unwrap();
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_path_filestat_get(&ctx, &mut caller, 3, 0, 0, -1, 256).unwrap();
        assert_eq!(result, WasiErrno::Inval.as_i32());

        let _ = std::fs::remove_dir_all(&dir);
    }

    // -- path_create_directory --

    #[test]
    fn test_path_create_directory() {
        let dir = std::env::temp_dir().join("kasm_test_path_mkdir");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();

        let ctx = ctx_with_preopen(&dir);
        let mut memory = Memory::new(1, None).unwrap();
        let (pp, pl) = write_path(&mut memory, 0, "newdir");
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_path_create_directory(&ctx, &mut caller, 3, pp, pl).unwrap();
        assert_eq!(result, WasiErrno::Success.as_i32());
        assert!(dir.join("newdir").is_dir());

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_path_create_directory_already_exists() {
        let dir = std::env::temp_dir().join("kasm_test_path_mkdir_exists");
        let _ = std::fs::create_dir_all(dir.join("existing"));

        let ctx = ctx_with_preopen(&dir);
        let mut memory = Memory::new(1, None).unwrap();
        let (pp, pl) = write_path(&mut memory, 0, "existing");
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_path_create_directory(&ctx, &mut caller, 3, pp, pl).unwrap();
        assert_ne!(result, WasiErrno::Success.as_i32(), "creating existing dir should fail");

        let _ = std::fs::remove_dir_all(&dir);
    }

    // -- path_remove_directory --

    #[test]
    fn test_path_remove_directory() {
        let dir = std::env::temp_dir().join("kasm_test_path_rmdir");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(dir.join("victim")).unwrap();

        let ctx = ctx_with_preopen(&dir);
        let mut memory = Memory::new(1, None).unwrap();
        let (pp, pl) = write_path(&mut memory, 0, "victim");
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_path_remove_directory(&ctx, &mut caller, 3, pp, pl).unwrap();
        assert_eq!(result, WasiErrno::Success.as_i32());
        assert!(!dir.join("victim").exists());

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_path_remove_directory_not_empty() {
        let dir = std::env::temp_dir().join("kasm_test_path_rmdir_notempty");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(dir.join("parent")).unwrap();
        std::fs::write(dir.join("parent/child.txt"), b"x").unwrap();

        let ctx = ctx_with_preopen(&dir);
        let mut memory = Memory::new(1, None).unwrap();
        let (pp, pl) = write_path(&mut memory, 0, "parent");
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_path_remove_directory(&ctx, &mut caller, 3, pp, pl).unwrap();
        assert_ne!(
            result,
            WasiErrno::Success.as_i32(),
            "removing non-empty dir should fail"
        );

        let _ = std::fs::remove_dir_all(&dir);
    }

    // -- path_unlink_file --

    #[test]
    fn test_path_unlink_file() {
        let dir = std::env::temp_dir().join("kasm_test_path_unlink");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("doomed.txt"), b"bye").unwrap();

        let ctx = ctx_with_preopen(&dir);
        let mut memory = Memory::new(1, None).unwrap();
        let (pp, pl) = write_path(&mut memory, 0, "doomed.txt");
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_path_unlink_file(&ctx, &mut caller, 3, pp, pl).unwrap();
        assert_eq!(result, WasiErrno::Success.as_i32());
        assert!(!dir.join("doomed.txt").exists());

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_path_unlink_nonexistent() {
        let dir = std::env::temp_dir().join("kasm_test_path_unlink_noent");
        let _ = std::fs::create_dir_all(&dir);

        let ctx = ctx_with_preopen(&dir);
        let mut memory = Memory::new(1, None).unwrap();
        let (pp, pl) = write_path(&mut memory, 0, "nope.txt");
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_path_unlink_file(&ctx, &mut caller, 3, pp, pl).unwrap();
        assert_ne!(result, WasiErrno::Success.as_i32());

        let _ = std::fs::remove_dir_all(&dir);
    }

    // -- path_rename --

    #[test]
    fn test_path_rename() {
        let dir = std::env::temp_dir().join("kasm_test_path_rename");
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("old.txt"), b"content").unwrap();

        let ctx = ctx_with_preopen(&dir);
        let mut memory = Memory::new(1, None).unwrap();
        let (op, ol) = write_path(&mut memory, 0, "old.txt");
        let (np, nl) = write_path(&mut memory, 64, "new.txt");
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_path_rename(&ctx, &mut caller, 3, op, ol, 3, np, nl).unwrap();
        assert_eq!(result, WasiErrno::Success.as_i32());
        assert!(!dir.join("old.txt").exists());
        assert!(dir.join("new.txt").exists());
        assert_eq!(std::fs::read(dir.join("new.txt")).unwrap(), b"content");

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_path_rename_nonexistent_source() {
        let dir = std::env::temp_dir().join("kasm_test_path_rename_noent");
        let _ = std::fs::create_dir_all(&dir);

        let ctx = ctx_with_preopen(&dir);
        let mut memory = Memory::new(1, None).unwrap();
        let (op, ol) = write_path(&mut memory, 0, "ghost.txt");
        let (np, nl) = write_path(&mut memory, 64, "dest.txt");
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let result = wasi_path_rename(&ctx, &mut caller, 3, op, ol, 3, np, nl).unwrap();
        assert_ne!(result, WasiErrno::Success.as_i32());

        let _ = std::fs::remove_dir_all(&dir);
    }

    // -- read_guest_path --

    #[test]
    fn test_read_guest_path_negative_ptr() {
        let mut memory = Memory::new(1, None).unwrap();
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let err = read_guest_path(&mut caller, -1, 5).unwrap_err();
        assert!(
            matches!(err, RuntimeError::MemoryError(_)),
            "negative ptr should be MemoryError"
        );
    }

    #[test]
    fn test_read_guest_path_negative_len() {
        let mut memory = Memory::new(1, None).unwrap();
        let mut data = ();
        let mut caller = Caller::for_test(Some(&mut memory), &mut data);

        let err = read_guest_path(&mut caller, 0, -1).unwrap_err();
        assert!(
            matches!(err, RuntimeError::MemoryError(_)),
            "negative len should be MemoryError"
        );
    }
}
