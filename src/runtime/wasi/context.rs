//! WASI context for managing WASI state
//!
//! This module provides the `WasiContext` which holds all WASI-related state
//! including file descriptors, arguments, and environment variables.

use super::types::{WasiErrno, WasiFileType};
use crate::runtime::{RuntimeError, SharedMemory};
use std::cell::RefCell;
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::PathBuf;

/// A file descriptor entry for WASI
pub struct FileDescriptor {
    /// The underlying reader (if readable)
    reader: Option<Box<dyn Read + Send>>,
    /// The underlying writer (if writable)
    writer: Option<Box<dyn Write + Send>>,
    /// The underlying seeker (if seekable)
    seeker: Option<Box<dyn Seek + Send>>,
    /// Whether this fd is readable
    pub readable: bool,
    /// Whether this fd is writable
    pub writable: bool,
    /// The WASI file type
    pub file_type: WasiFileType,
}

impl FileDescriptor {
    /// Create a new readable file descriptor (character device, e.g. stdin)
    pub fn new_reader(reader: Box<dyn Read + Send>) -> Self {
        Self {
            reader: Some(reader),
            writer: None,
            seeker: None,
            readable: true,
            writable: false,
            file_type: WasiFileType::CharacterDevice,
        }
    }

    /// Create a new writable file descriptor (character device, e.g. stdout/stderr)
    pub fn new_writer(writer: Box<dyn Write + Send>) -> Self {
        Self {
            reader: None,
            writer: Some(writer),
            seeker: None,
            readable: false,
            writable: true,
            file_type: WasiFileType::CharacterDevice,
        }
    }

    /// Create a file descriptor backed by a real file
    ///
    /// Uses `try_clone()` to obtain separate handles for reading, writing, and seeking.
    pub fn new_file(file: std::fs::File, readable: bool, writable: bool) -> std::io::Result<Self> {
        let reader = if readable {
            Some(Box::new(file.try_clone()?) as Box<dyn Read + Send>)
        } else {
            None
        };
        let writer = if writable {
            Some(Box::new(file.try_clone()?) as Box<dyn Write + Send>)
        } else {
            None
        };
        let seeker = Some(Box::new(file) as Box<dyn Seek + Send>);
        Ok(Self {
            reader,
            writer,
            seeker,
            readable,
            writable,
            file_type: WasiFileType::RegularFile,
        })
    }

    /// Create a directory file descriptor (for preopens)
    pub fn new_directory() -> Self {
        Self {
            reader: None,
            writer: None,
            seeker: None,
            readable: false,
            writable: false,
            file_type: WasiFileType::Directory,
        }
    }

    /// Read from this file descriptor
    pub fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        match &mut self.reader {
            Some(reader) => reader.read(buf),
            None => Err(std::io::Error::new(
                std::io::ErrorKind::PermissionDenied,
                "fd is not readable",
            )),
        }
    }

    /// Write to this file descriptor
    pub fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match &mut self.writer {
            Some(writer) => writer.write(buf),
            None => Err(std::io::Error::new(
                std::io::ErrorKind::PermissionDenied,
                "fd is not writable",
            )),
        }
    }

    /// Flush this file descriptor
    pub fn flush(&mut self) -> std::io::Result<()> {
        match &mut self.writer {
            Some(writer) => writer.flush(),
            None => Ok(()),
        }
    }

    /// Seek within this file descriptor
    pub fn seek(&mut self, pos: SeekFrom) -> std::io::Result<u64> {
        match &mut self.seeker {
            Some(seeker) => seeker.seek(pos),
            None => Err(std::io::Error::new(
                std::io::ErrorKind::Unsupported,
                "fd is not seekable",
            )),
        }
    }
}

impl std::fmt::Debug for FileDescriptor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FileDescriptor")
            .field("readable", &self.readable)
            .field("writable", &self.writable)
            .field("file_type", &self.file_type)
            .finish()
    }
}

/// WASI context holding all WASI-related state
///
/// The context is shared between all WASI functions via `Arc<WasiContext>`.
/// Memory is bound lazily after module instantiation.
pub struct WasiContext {
    /// The WebAssembly linear memory (bound after instantiation)
    memory: RefCell<Option<SharedMemory>>,
    /// File descriptors (0=stdin, 1=stdout, 2=stderr, 3+=preopens/files)
    fds: RefCell<Vec<Option<FileDescriptor>>>,
    /// Preopened directories: (fd number, guest path, host path)
    preopens: Vec<(u32, String, PathBuf)>,
    /// Command line arguments
    args: Vec<String>,
    /// Environment variables (name=value pairs)
    env: Vec<String>,
    /// Process exit code (set by proc_exit)
    exit_code: RefCell<Option<i32>>,
}

impl WasiContext {
    /// Create a new builder for WasiContext
    pub fn builder() -> WasiContextBuilder {
        WasiContextBuilder::new()
    }

    /// Bind the linear memory to this context
    ///
    /// This must be called after module instantiation but before
    /// any WASI functions are invoked.
    pub fn bind_memory(&self, memory: SharedMemory) {
        *self.memory.borrow_mut() = Some(memory);
    }

    /// Check if memory is bound
    pub fn has_memory(&self) -> bool {
        self.memory.borrow().is_some()
    }

    /// Get the exit code if proc_exit was called
    pub fn exit_code(&self) -> Option<i32> {
        *self.exit_code.borrow()
    }

    /// Set the exit code (called by proc_exit)
    pub fn set_exit_code(&self, code: i32) {
        *self.exit_code.borrow_mut() = Some(code);
    }

    /// Get a reference to the args
    pub fn args(&self) -> &[String] {
        &self.args
    }

    /// Get a reference to the environment variables
    pub fn env(&self) -> &[String] {
        &self.env
    }

    /// Borrow the file descriptors for reading
    pub fn fds_ref(&self) -> std::cell::Ref<'_, Vec<Option<FileDescriptor>>> {
        self.fds.borrow()
    }

    // === Memory helper methods ===

    /// Read a u32 from linear memory
    pub fn read_u32(&self, addr: u32) -> Result<u32, RuntimeError> {
        let memory_ref = self.memory.borrow();
        let shared_memory = memory_ref
            .as_ref()
            .ok_or_else(|| RuntimeError::MemoryError("WASI memory not bound".to_string()))?;
        let memory = shared_memory
            .lock()
            .map_err(|_| RuntimeError::MemoryError("Failed to lock memory".to_string()))?;
        memory.read_u32(addr)
    }

    /// Write a u32 to linear memory
    pub fn write_u32(&self, addr: u32, value: u32) -> Result<(), RuntimeError> {
        let memory_ref = self.memory.borrow();
        let shared_memory = memory_ref
            .as_ref()
            .ok_or_else(|| RuntimeError::MemoryError("WASI memory not bound".to_string()))?;
        let mut memory = shared_memory
            .lock()
            .map_err(|_| RuntimeError::MemoryError("Failed to lock memory".to_string()))?;
        memory.write_u32(addr, value)
    }

    /// Read bytes from linear memory
    pub fn read_bytes(&self, addr: u32, len: usize) -> Result<Vec<u8>, RuntimeError> {
        let memory_ref = self.memory.borrow();
        let shared_memory = memory_ref
            .as_ref()
            .ok_or_else(|| RuntimeError::MemoryError("WASI memory not bound".to_string()))?;
        let memory = shared_memory
            .lock()
            .map_err(|_| RuntimeError::MemoryError("Failed to lock memory".to_string()))?;
        memory.read_bytes(addr, len)
    }

    /// Write bytes to linear memory
    pub fn write_bytes(&self, addr: u32, bytes: &[u8]) -> Result<(), RuntimeError> {
        let memory_ref = self.memory.borrow();
        let shared_memory = memory_ref
            .as_ref()
            .ok_or_else(|| RuntimeError::MemoryError("WASI memory not bound".to_string()))?;
        let mut memory = shared_memory
            .lock()
            .map_err(|_| RuntimeError::MemoryError("Failed to lock memory".to_string()))?;
        memory.write_bytes(addr, bytes)
    }

    /// Write a u64 to linear memory (little-endian)
    pub fn write_u64(&self, addr: u32, value: u64) -> Result<(), RuntimeError> {
        self.write_bytes(addr, &value.to_le_bytes())
    }

    // === Preopen methods ===

    /// Get the guest path for a preopened fd
    pub fn get_preopen(&self, fd: u32) -> Option<&str> {
        self.preopens
            .iter()
            .find(|(preopen_fd, _, _)| *preopen_fd == fd)
            .map(|(_, path, _)| path.as_str())
    }

    /// Get the list of preopens as (fd, guest_path) pairs
    pub fn preopens(&self) -> Vec<(u32, &str)> {
        self.preopens
            .iter()
            .map(|(fd, guest, _)| (*fd, guest.as_str()))
            .collect()
    }

    /// Resolve a path relative to a preopened directory
    ///
    /// Verifies the resolved path does not escape the preopen directory.
    pub fn resolve_path(&self, dir_fd: u32, path: &str) -> Result<PathBuf, WasiErrno> {
        let (_, _, host_dir) = self
            .preopens
            .iter()
            .find(|(fd, _, _)| *fd == dir_fd)
            .ok_or(WasiErrno::BadF)?;

        let resolved = host_dir.join(path);

        // Canonicalise the parent directory to check for traversal
        // (the file itself may not exist yet for O_CREAT)
        let canonical_dir = host_dir.canonicalize().map_err(|_| WasiErrno::NoEnt)?;

        let check_path = if resolved.exists() {
            resolved.canonicalize().map_err(|_| WasiErrno::NoEnt)?
        } else {
            // For non-existent files, canonicalise the parent
            let parent = resolved.parent().ok_or(WasiErrno::NoEnt)?;
            let canonical_parent = parent.canonicalize().map_err(|_| WasiErrno::NoEnt)?;
            let filename = resolved.file_name().ok_or(WasiErrno::Inval)?;
            canonical_parent.join(filename)
        };

        if !check_path.starts_with(&canonical_dir) {
            return Err(WasiErrno::Access);
        }

        Ok(resolved)
    }

    // === File descriptor management ===

    /// Allocate a new file descriptor, returning its number.
    ///
    /// Reuses closed (None) slots before appending to avoid unbounded growth.
    pub fn allocate_fd(&self, fd: FileDescriptor) -> u32 {
        let mut fds = self.fds.borrow_mut();
        // Reuse a closed slot if available (skip stdin/stdout/stderr)
        for (i, slot) in fds.iter_mut().enumerate().skip(3) {
            if slot.is_none() {
                *slot = Some(fd);
                return i as u32;
            }
        }
        let idx = fds.len();
        fds.push(Some(fd));
        idx as u32
    }

    /// Close a file descriptor
    pub fn close_fd(&self, fd: u32) -> Result<(), WasiErrno> {
        let mut fds = self.fds.borrow_mut();
        let entry = fds.get_mut(fd as usize).ok_or(WasiErrno::BadF)?;
        if entry.is_none() {
            return Err(WasiErrno::BadF);
        }
        *entry = None;
        Ok(())
    }

    /// Seek within a file descriptor
    pub fn fd_seek(&self, fd: u32, offset: i64, whence: u32) -> Result<u64, WasiErrno> {
        let seek_from = match whence {
            0 => {
                if offset < 0 {
                    return Err(WasiErrno::Inval);
                }
                SeekFrom::Start(offset as u64)
            }
            1 => SeekFrom::Current(offset),
            2 => SeekFrom::End(offset),
            _ => return Err(WasiErrno::Inval),
        };

        let mut fds = self.fds.borrow_mut();
        let fd_entry = fds
            .get_mut(fd as usize)
            .ok_or(WasiErrno::BadF)?
            .as_mut()
            .ok_or(WasiErrno::BadF)?;

        fd_entry.seek(seek_from).map_err(|_| WasiErrno::Spipe)
    }

    // === File descriptor I/O methods ===

    /// Read from a file descriptor into memory
    ///
    /// Returns the number of bytes read or a WASI errno.
    pub fn fd_read(&self, fd: u32, iovs_ptr: u32, iovs_len: u32) -> Result<usize, WasiErrno> {
        // Read iovec structures from memory
        let iovecs = self.read_iovecs(iovs_ptr, iovs_len)?;

        let mut fds = self.fds.borrow_mut();
        let fd_entry = fds
            .get_mut(fd as usize)
            .ok_or(WasiErrno::BadF)?
            .as_mut()
            .ok_or(WasiErrno::BadF)?;

        if !fd_entry.readable {
            return Err(WasiErrno::BadF);
        }

        let mut total_read = 0;
        for (buf_ptr, buf_len) in iovecs {
            if buf_len == 0 {
                continue;
            }

            // Read into a temporary buffer
            let mut temp_buf = vec![0u8; buf_len as usize];
            match fd_entry.read(&mut temp_buf) {
                Ok(0) => break, // EOF
                Ok(n) => {
                    // Write the read data to memory
                    self.write_bytes(buf_ptr, &temp_buf[..n])
                        .map_err(|_| WasiErrno::Fault)?;
                    total_read += n;
                    if n < buf_len as usize {
                        break; // Partial read, stop here
                    }
                }
                Err(e) if e.kind() == std::io::ErrorKind::Interrupted => continue,
                Err(_) => return Err(WasiErrno::Io),
            }
        }

        Ok(total_read)
    }

    /// Write from memory to a file descriptor
    ///
    /// Returns the number of bytes written or a WASI errno.
    pub fn fd_write(&self, fd: u32, iovs_ptr: u32, iovs_len: u32) -> Result<usize, WasiErrno> {
        // Read iovec structures from memory
        let iovecs = self.read_iovecs(iovs_ptr, iovs_len)?;

        let mut fds = self.fds.borrow_mut();
        let fd_entry = fds
            .get_mut(fd as usize)
            .ok_or(WasiErrno::BadF)?
            .as_mut()
            .ok_or(WasiErrno::BadF)?;

        if !fd_entry.writable {
            return Err(WasiErrno::BadF);
        }

        let mut total_written = 0;
        for (buf_ptr, buf_len) in iovecs {
            if buf_len == 0 {
                continue;
            }

            // Read data from memory
            let data = self
                .read_bytes(buf_ptr, buf_len as usize)
                .map_err(|_| WasiErrno::Fault)?;

            // Write to the file descriptor
            match fd_entry.write(&data) {
                Ok(n) => {
                    total_written += n;
                    if n < buf_len as usize {
                        break; // Partial write, stop here
                    }
                }
                Err(e) if e.kind() == std::io::ErrorKind::Interrupted => continue,
                Err(_) => return Err(WasiErrno::Io),
            }
        }

        // Flush after writing
        let _ = fd_entry.flush();

        Ok(total_written)
    }

    /// Read iovec structures from memory.
    ///
    /// Each iovec is 8 bytes: 4-byte pointer (buf) + 4-byte length (buf_len).
    ///
    /// See: <https://github.com/WebAssembly/WASI/blob/wasi-0.1/preview1/docs.md#iovec>
    fn read_iovecs(&self, ptr: u32, len: u32) -> Result<Vec<(u32, u32)>, WasiErrno> {
        let mut iovecs = Vec::with_capacity(len as usize);
        for i in 0..len {
            let base = ptr + i * 8;
            let buf_ptr = self.read_u32(base).map_err(|_| WasiErrno::Fault)?;
            let buf_len = self.read_u32(base + 4).map_err(|_| WasiErrno::Fault)?;
            iovecs.push((buf_ptr, buf_len));
        }
        Ok(iovecs)
    }
}

impl std::fmt::Debug for WasiContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("WasiContext")
            .field("has_memory", &self.has_memory())
            .field("args", &self.args)
            .field("env", &self.env)
            .field("exit_code", &self.exit_code)
            .finish()
    }
}

/// Builder for WasiContext
pub struct WasiContextBuilder {
    args: Vec<String>,
    env: Vec<String>,
    stdin: Option<Box<dyn Read + Send>>,
    stdout: Option<Box<dyn Write + Send>>,
    stderr: Option<Box<dyn Write + Send>>,
    preopens: Vec<(String, String)>,
}

impl WasiContextBuilder {
    /// Create a new builder with default settings
    pub fn new() -> Self {
        Self {
            args: Vec::new(),
            env: Vec::new(),
            stdin: None,
            stdout: None,
            stderr: None,
            preopens: Vec::new(),
        }
    }

    /// Set command line arguments
    pub fn args(mut self, args: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.args = args.into_iter().map(|s| s.into()).collect();
        self
    }

    /// Set environment variables (as "NAME=value" strings)
    pub fn env(mut self, env: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.env = env.into_iter().map(|s| s.into()).collect();
        self
    }

    /// Set stdin
    pub fn stdin(mut self, stdin: Box<dyn Read + Send>) -> Self {
        self.stdin = Some(stdin);
        self
    }

    /// Set stdout
    pub fn stdout(mut self, stdout: Box<dyn Write + Send>) -> Self {
        self.stdout = Some(stdout);
        self
    }

    /// Set stderr
    pub fn stderr(mut self, stderr: Box<dyn Write + Send>) -> Self {
        self.stderr = Some(stderr);
        self
    }

    /// Add a preopened directory
    ///
    /// `host_path` is the path on the host filesystem.
    /// `guest_path` is the path the WASI program sees.
    pub fn preopen_dir(mut self, host_path: impl Into<String>, guest_path: impl Into<String>) -> Self {
        self.preopens.push((host_path.into(), guest_path.into()));
        self
    }

    /// Build the WasiContext
    pub fn build(self) -> WasiContext {
        let mut fds: Vec<Option<FileDescriptor>> = vec![
            // fd 0 = stdin
            self.stdin.map(FileDescriptor::new_reader),
            // fd 1 = stdout
            self.stdout.map(FileDescriptor::new_writer),
            // fd 2 = stderr
            self.stderr.map(FileDescriptor::new_writer),
        ];

        let mut preopens = Vec::new();
        for (host_path, guest_path) in &self.preopens {
            let path = PathBuf::from(host_path);
            if path.is_dir() {
                let fd_num = fds.len() as u32;
                fds.push(Some(FileDescriptor::new_directory()));
                preopens.push((fd_num, guest_path.clone(), path));
            }
        }

        WasiContext {
            memory: RefCell::new(None),
            fds: RefCell::new(fds),
            preopens,
            args: self.args,
            env: self.env,
            exit_code: RefCell::new(None),
        }
    }
}

impl Default for WasiContextBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::Memory;
    use std::io::Cursor;
    use std::sync::{Arc, Mutex};

    #[test]
    fn test_builder_creates_context_with_args() {
        let ctx = WasiContext::builder().args(["program", "arg1", "arg2"]).build();

        assert_eq!(ctx.args(), ["program", "arg1", "arg2"]);
    }

    #[test]
    fn test_builder_creates_context_with_env() {
        let ctx = WasiContext::builder().env(["PATH=/usr/bin", "HOME=/home/user"]).build();

        assert_eq!(ctx.env(), ["PATH=/usr/bin", "HOME=/home/user"]);
    }

    #[test]
    fn test_memory_binding() {
        let ctx = WasiContext::builder().build();

        assert!(!ctx.has_memory());

        let memory = Memory::new(1, None).unwrap();
        let shared_memory = Arc::new(Mutex::new(memory));
        ctx.bind_memory(shared_memory);

        assert!(ctx.has_memory());
    }

    #[test]
    fn test_read_write_u32() {
        let ctx = WasiContext::builder().build();

        let memory = Memory::new(1, None).unwrap();
        let shared_memory = Arc::new(Mutex::new(memory));
        ctx.bind_memory(shared_memory);

        // Write and read back
        ctx.write_u32(100, 0x12345678).unwrap();
        let value = ctx.read_u32(100).unwrap();
        assert_eq!(value, 0x12345678);
    }

    #[test]
    fn test_read_write_bytes() {
        let ctx = WasiContext::builder().build();

        let memory = Memory::new(1, None).unwrap();
        let shared_memory = Arc::new(Mutex::new(memory));
        ctx.bind_memory(shared_memory);

        let data = b"Hello, WASI!";
        ctx.write_bytes(200, data).unwrap();
        let read_data = ctx.read_bytes(200, data.len()).unwrap();
        assert_eq!(read_data, data);
    }

    #[test]
    fn test_exit_code() {
        let ctx = WasiContext::builder().build();

        assert!(ctx.exit_code().is_none());

        ctx.set_exit_code(42);
        assert_eq!(ctx.exit_code(), Some(42));
    }

    #[test]
    fn test_fd_write_to_captured_stdout() {
        // Create a captured stdout
        let stdout_buffer = Arc::new(Mutex::new(Vec::<u8>::new()));
        let stdout_clone = stdout_buffer.clone();

        struct CapturedWriter(Arc<Mutex<Vec<u8>>>);
        impl Write for CapturedWriter {
            fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
                self.0.lock().unwrap().extend_from_slice(buf);
                Ok(buf.len())
            }
            fn flush(&mut self) -> std::io::Result<()> {
                Ok(())
            }
        }

        let ctx = WasiContext::builder()
            .stdout(Box::new(CapturedWriter(stdout_clone)))
            .build();

        // Bind memory
        let memory = Memory::new(1, None).unwrap();
        let shared_memory = Arc::new(Mutex::new(memory));
        ctx.bind_memory(shared_memory);

        // Set up iovec in memory: {ptr: 100, len: 13}
        let message = b"Hello, World!";
        ctx.write_bytes(100, message).unwrap();
        ctx.write_u32(0, 100).unwrap(); // iovec.ptr
        ctx.write_u32(4, 13).unwrap(); // iovec.len

        // Call fd_write
        let result = ctx.fd_write(1, 0, 1);
        assert_eq!(result, Ok(13));

        // Check captured output
        let output = stdout_buffer.lock().unwrap();
        assert_eq!(&*output, b"Hello, World!");
    }

    #[test]
    fn test_fd_read_from_mock_stdin() {
        // Create a mock stdin with some data
        let input = Cursor::new(b"test input\n".to_vec());

        let ctx = WasiContext::builder().stdin(Box::new(input)).build();

        // Bind memory
        let memory = Memory::new(1, None).unwrap();
        let shared_memory = Arc::new(Mutex::new(memory));
        ctx.bind_memory(shared_memory);

        // Set up iovec in memory: {ptr: 100, len: 64}
        ctx.write_u32(0, 100).unwrap(); // iovec.ptr
        ctx.write_u32(4, 64).unwrap(); // iovec.len

        // Call fd_read
        let result = ctx.fd_read(0, 0, 1);
        assert_eq!(result, Ok(11)); // "test input\n" is 11 bytes

        // Check data was written to memory
        let data = ctx.read_bytes(100, 11).unwrap();
        assert_eq!(&data, b"test input\n");
    }

    #[test]
    fn test_fd_read_eof() {
        // Create an empty stdin
        let input = Cursor::new(Vec::<u8>::new());

        let ctx = WasiContext::builder().stdin(Box::new(input)).build();

        // Bind memory
        let memory = Memory::new(1, None).unwrap();
        let shared_memory = Arc::new(Mutex::new(memory));
        ctx.bind_memory(shared_memory);

        // Set up iovec
        ctx.write_u32(0, 100).unwrap();
        ctx.write_u32(4, 64).unwrap();

        // Call fd_read - should return 0 for EOF
        let result = ctx.fd_read(0, 0, 1);
        assert_eq!(result, Ok(0));
    }

    #[test]
    fn test_bad_fd_returns_error() {
        let ctx = WasiContext::builder().build();

        // Bind memory
        let memory = Memory::new(1, None).unwrap();
        let shared_memory = Arc::new(Mutex::new(memory));
        ctx.bind_memory(shared_memory);

        ctx.write_u32(0, 100).unwrap();
        ctx.write_u32(4, 64).unwrap();

        // fd 99 doesn't exist
        let result = ctx.fd_read(99, 0, 1);
        assert_eq!(result, Err(WasiErrno::BadF));
    }
}
