// stat - display file metadata
//
// Exercises path_filestat_get and fd_filestat_get to read and display
// file type and size information.
//
// Usage: kasm run --dir=. stat.wasm -- filename.txt
//
// Output format:
//   path: filename.txt
//   type: regular
//   size: 14

import {
  fd_write,
  fd_close,
  args_sizes_get,
  args_get,
  fd_prestat_get,
  path_open,
  path_filestat_get,
  fd_filestat_get,
  STDOUT,
  STDERR,
  ERRNO_SUCCESS,
} from "./env";

const BASE: i32 = 60 * 1024;

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
const PATH_BUF: i32 = BASE + 400;

// Filestat buffer: 64 bytes
//   dev: u64 (0)
//   ino: u64 (8)
//   filetype: u8 (16) + 7 padding
//   nlink: u64 (24)
//   size: u64 (32)
//   atim: u64 (40)
//   mtim: u64 (48)
//   ctim: u64 (56)
const FILESTAT_BUF: i32 = BASE + 500;

// WASI file types
const FILETYPE_UNKNOWN: u8 = 0;
const FILETYPE_BLOCK_DEVICE: u8 = 1;
const FILETYPE_CHARACTER_DEVICE: u8 = 2;
const FILETYPE_DIRECTORY: u8 = 3;
const FILETYPE_REGULAR_FILE: u8 = 4;
const FILETYPE_SOCKET_DGRAM: u8 = 5;
const FILETYPE_SOCKET_STREAM: u8 = 6;
const FILETYPE_SYMBOLIC_LINK: u8 = 7;

// The encoded ArrayBuffer lives on the AS heap which is part of linear memory,
// so its pointer is valid for fd_write to read from directly.
function writeStr(fd: i32, msg: string): void {
  const encoded = String.UTF8.encode(msg);
  store<i32>(IOVEC_BUF_PTR, changetype<i32>(encoded));
  store<i32>(IOVEC_BUF_LEN, encoded.byteLength);
  fd_write(fd, IOVECS_PTR, IOVECS_COUNT, RESULT_PTR);
}

function copyPath(dest: i32, src: i32, len: i32): void {
  memory.copy(dest, src, len);
}

function fileTypeName(ft: u8): string {
  if (ft == FILETYPE_REGULAR_FILE) return "regular";
  if (ft == FILETYPE_DIRECTORY) return "directory";
  if (ft == FILETYPE_SYMBOLIC_LINK) return "symlink";
  if (ft == FILETYPE_CHARACTER_DEVICE) return "chardev";
  if (ft == FILETYPE_BLOCK_DEVICE) return "blockdev";
  if (ft == FILETYPE_SOCKET_DGRAM) return "socket_dgram";
  if (ft == FILETYPE_SOCKET_STREAM) return "socket_stream";
  return "unknown";
}

export function _start(): void {
  // Get args
  args_sizes_get(ARGC_PTR, ARGV_BUF_SIZE_PTR);
  const argc = load<i32>(ARGC_PTR);
  args_get(ARGV_PTR, ARGV_BUF_PTR);

  if (argc < 2) {
    writeStr(STDERR, "Usage: stat <path>\n");
    return;
  }

  // Get path from argv[1]
  const pathArgPtr = load<i32>(ARGV_PTR + PTR_SIZE);
  let pathArgLen: i32 = 0;
  while (load<u8>(pathArgPtr + pathArgLen) != 0) {
    pathArgLen++;
  }
  const pathStr = String.UTF8.decodeUnsafe(pathArgPtr, pathArgLen);

  // Find preopen (fd 3)
  const err = fd_prestat_get(3, PRESTAT_BUF);
  if (err != ERRNO_SUCCESS) {
    writeStr(STDERR, "Error: no preopened directories (use --dir)\n");
    return;
  }

  // Use path_filestat_get to stat via the preopen
  copyPath(PATH_BUF, pathArgPtr, pathArgLen);
  const statErr = path_filestat_get(3, 0, PATH_BUF, pathArgLen, FILESTAT_BUF);
  if (statErr != ERRNO_SUCCESS) {
    writeStr(STDERR, "Error: path_filestat_get failed (errno " + statErr.toString() + ")\n");
    return;
  }

  const filetype = load<u8>(FILESTAT_BUF + 16);
  const size = load<i64>(FILESTAT_BUF + 32);

  writeStr(STDOUT, "path: " + pathStr + "\n");
  writeStr(STDOUT, "type: " + fileTypeName(filetype) + "\n");
  writeStr(STDOUT, "size: " + size.toString() + "\n");

  // Also verify fd_filestat_get produces the same result by opening the file
  if (filetype == FILETYPE_REGULAR_FILE) {
    const openErr = path_open(
      3, 0, PATH_BUF, pathArgLen,
      0, <i64>0x1fffffff, <i64>0x1fffffff, 0,
      OPENED_FD_PTR
    );
    if (openErr == ERRNO_SUCCESS) {
      const fd = load<i32>(OPENED_FD_PTR);
      const fdStatErr = fd_filestat_get(fd, FILESTAT_BUF);
      if (fdStatErr == ERRNO_SUCCESS) {
        const fdSize = load<i64>(FILESTAT_BUF + 32);
        writeStr(STDOUT, "fd_size: " + fdSize.toString() + "\n");
      }
      fd_close(fd);
    }
  }
}
