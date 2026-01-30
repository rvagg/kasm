// WASI preview1 function declarations
// These are imported from the host environment

@external("wasi_snapshot_preview1", "fd_read")
export declare function fd_read(fd: i32, iovs: i32, iovs_len: i32, nread: i32): i32;

@external("wasi_snapshot_preview1", "fd_write")
export declare function fd_write(fd: i32, iovs: i32, iovs_len: i32, nwritten: i32): i32;

@external("wasi_snapshot_preview1", "fd_close")
export declare function fd_close(fd: i32): i32;

@external("wasi_snapshot_preview1", "args_sizes_get")
export declare function args_sizes_get(argc: i32, argv_buf_size: i32): i32;

@external("wasi_snapshot_preview1", "args_get")
export declare function args_get(argv: i32, argv_buf: i32): i32;

@external("wasi_snapshot_preview1", "environ_sizes_get")
export declare function environ_sizes_get(environc: i32, environ_buf_size: i32): i32;

@external("wasi_snapshot_preview1", "environ_get")
export declare function environ_get(environ: i32, environ_buf: i32): i32;

@external("wasi_snapshot_preview1", "proc_exit")
export declare function proc_exit(code: i32): void;

@external("wasi_snapshot_preview1", "fd_prestat_get")
export declare function fd_prestat_get(fd: i32, buf: i32): i32;

@external("wasi_snapshot_preview1", "fd_prestat_dir_name")
export declare function fd_prestat_dir_name(fd: i32, path: i32, path_len: i32): i32;

@external("wasi_snapshot_preview1", "path_open")
export declare function path_open(
  fd: i32, dirflags: i32, path: i32, path_len: i32,
  oflags: i32, rights_base: i64, rights_inheriting: i64,
  fdflags: i32, opened_fd: i32
): i32;

@external("wasi_snapshot_preview1", "fd_fdstat_get")
export declare function fd_fdstat_get(fd: i32, buf: i32): i32;

@external("wasi_snapshot_preview1", "fd_seek")
export declare function fd_seek(fd: i32, offset: i64, whence: i32, newoffset: i32): i32;

// File descriptor constants
export const STDIN: i32 = 0;
export const STDOUT: i32 = 1;
export const STDERR: i32 = 2;

// WASI error codes
export const ERRNO_SUCCESS: i32 = 0;
export const ERRNO_BADF: i32 = 8;
export const ERRNO_INVAL: i32 = 28;
export const ERRNO_NOSYS: i32 = 52;
