// Cat - read a file and write its contents to stdout
//
// Usage: kasm run --dir=. cat.wasm -- filename.txt

import {
  fd_read,
  fd_write,
  fd_close,
  args_sizes_get,
  args_get,
  fd_prestat_get,
  fd_prestat_dir_name,
  path_open,
  STDOUT,
  STDERR,
  ERRNO_SUCCESS,
} from "./env";

// Memory layout: use high offsets to avoid colliding with AssemblyScript's heap.
// AS stub runtime heap grows upward from __heap_base (typically ~1KB-4KB).
// We place our scratch memory at 60KB (near top of first 64KB page).
const BASE: i32 = 60 * 1024; // 61440

const IOVEC_BUF_PTR: i32 = BASE;
const IOVEC_BUF_LEN: i32 = BASE + 4;
const RESULT_PTR: i32 = BASE + 8;
const IOVECS_PTR: i32 = BASE;
const IOVECS_COUNT: i32 = 1;

const ARGC_PTR: i32 = BASE + 12;
const ARGV_BUF_SIZE_PTR: i32 = BASE + 16;
const ARGV_PTR: i32 = BASE + 20;
const ARGV_BUF_PTR: i32 = BASE + 40;
const PTR_SIZE: i32 = 4;

const PRESTAT_BUF: i32 = BASE + 200;
const OPENED_FD_PTR: i32 = BASE + 300;
// Read buffer: 2KB at BASE + 512
const DATA_BUF: i32 = BASE + 512;
const DATA_SIZE: i32 = 2048;

function writeStderr(msg: string): void {
  const encoded = String.UTF8.encode(msg);
  store<i32>(IOVEC_BUF_PTR, changetype<i32>(encoded));
  store<i32>(IOVEC_BUF_LEN, encoded.byteLength);
  fd_write(STDERR, IOVECS_PTR, IOVECS_COUNT, RESULT_PTR);
}

function writeStdout(ptr: i32, len: i32): void {
  store<i32>(IOVEC_BUF_PTR, ptr);
  store<i32>(IOVEC_BUF_LEN, len);
  fd_write(STDOUT, IOVECS_PTR, IOVECS_COUNT, RESULT_PTR);
}

export function _start(): void {
  // Get args
  args_sizes_get(ARGC_PTR, ARGV_BUF_SIZE_PTR);
  const argc = load<i32>(ARGC_PTR);
  args_get(ARGV_PTR, ARGV_BUF_PTR);

  if (argc < 2) {
    writeStderr("Usage: cat <filename>\n");
    return;
  }

  // Get filename from argv[1]
  const filenamePtr = load<i32>(ARGV_PTR + PTR_SIZE);
  let filenameLen: i32 = 0;
  while (load<u8>(filenamePtr + filenameLen) != 0) {
    filenameLen++;
  }

  // Find a preopen directory (fd 3)
  let preopenFd: i32 = -1;
  const err = fd_prestat_get(3, PRESTAT_BUF);
  if (err == ERRNO_SUCCESS) {
    preopenFd = 3;
  }

  if (preopenFd < 0) {
    writeStderr("Error: no preopened directories (use --dir)\n");
    return;
  }

  // Open the file relative to the preopen dir
  // rights: all read rights
  const openErr = path_open(
    preopenFd,
    0,               // dirflags
    filenamePtr,
    filenameLen,
    0,               // oflags
    <i64>0x1fffffff, // rights_base (all)
    <i64>0x1fffffff, // rights_inheriting
    0,               // fdflags
    OPENED_FD_PTR
  );

  if (openErr != ERRNO_SUCCESS) {
    writeStderr("Error: could not open file (errno " + openErr.toString() + ")\n");
    return;
  }

  const fileFd = load<i32>(OPENED_FD_PTR);

  // Read and write loop
  while (true) {
    store<i32>(IOVEC_BUF_PTR, DATA_BUF);
    store<i32>(IOVEC_BUF_LEN, DATA_SIZE);
    const readErr = fd_read(fileFd, IOVECS_PTR, IOVECS_COUNT, RESULT_PTR);
    if (readErr != ERRNO_SUCCESS) {
      break;
    }
    const nread = load<i32>(RESULT_PTR);
    if (nread == 0) {
      break; // EOF
    }
    writeStdout(DATA_BUF, nread);
  }

  fd_close(fileFd);
}
