// tree - recursively list a directory tree
//
// Lists directories and files in a tree format similar to unix `tree`.
// Exercises fd_readdir, path_open (for subdirectories), fd_close.
//
// Usage: kasm run --dir=/tmp/testdir tree.wasm [-- subdir]
//
// If a subdir argument is given, lists from that path within the preopen.
// Otherwise lists the root of the preopened directory.
//
// Output format (sorted, deterministic):
//   sub
//   sub/deep
//   sub/deep/nested.txt
//   sub/empty-dir
//   sub/greeting.txt

import {
  fd_write,
  fd_close,
  fd_readdir,
  args_sizes_get,
  args_get,
  fd_prestat_get,
  path_open,
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
const BUFUSED_PTR: i32 = BASE + 600;

// Readdir buffer: 4KB
const READDIR_BUF: i32 = BASE + 700;
const READDIR_BUF_LEN: i32 = 4096;

// Dirent header is 24 bytes:
//   d_next: u64 (8 bytes) - cookie of next entry
//   d_ino: u64 (8 bytes) - inode
//   d_namlen: u32 (4 bytes) - name length
//   d_type: u8 (1 byte) + 3 padding
const DIRENT_SIZE: i32 = 24;

// WASI file types
const FILETYPE_DIRECTORY: u8 = 3;

// The encoded ArrayBuffer lives on the AS heap which is part of linear memory,
// so its pointer is valid for fd_write to read from directly.
function writeStr(fd: i32, msg: string): void {
  const encoded = String.UTF8.encode(msg);
  store<i32>(IOVEC_BUF_PTR, changetype<i32>(encoded));
  store<i32>(IOVEC_BUF_LEN, encoded.byteLength);
  fd_write(fd, IOVECS_PTR, IOVECS_COUNT, RESULT_PTR);
}

function writeRaw(fd: i32, ptr: i32, len: i32): void {
  store<i32>(IOVEC_BUF_PTR, ptr);
  store<i32>(IOVEC_BUF_LEN, len);
  fd_write(fd, IOVECS_PTR, IOVECS_COUNT, RESULT_PTR);
}

function copyPath(dest: i32, path: string): i32 {
  const encoded = String.UTF8.encode(path);
  const len = encoded.byteLength;
  memory.copy(dest, changetype<i32>(encoded), len);
  return len;
}

// Open a directory relative to the preopen fd
function openDir(preopenFd: i32, pathPtr: i32, pathLen: i32): i32 {
  // O_DIRECTORY = 2 in oflags
  const err = path_open(
    preopenFd, 0, pathPtr, pathLen,
    2, <i64>0x1fffffff, <i64>0x1fffffff, 0,
    OPENED_FD_PTR
  );
  if (err != ERRNO_SUCCESS) return -1;
  return load<i32>(OPENED_FD_PTR);
}

// Read directory entry names and types from an fd.
// Collects entries into parallel arrays of names and types.
// Returns count of entries.
function readEntries(dirFd: i32, names: string[], types: u8[]): void {
  let cookie: i64 = 0;

  while (true) {
    const err = fd_readdir(dirFd, READDIR_BUF, READDIR_BUF_LEN, cookie, BUFUSED_PTR);
    if (err != ERRNO_SUCCESS) break;

    const bufused = load<i32>(BUFUSED_PTR);
    if (bufused == 0) break;

    let offset: i32 = 0;
    while (offset + DIRENT_SIZE <= bufused) {
      const nextCookie = load<i64>(READDIR_BUF + offset);
      const namelen = load<u32>(READDIR_BUF + offset + 16);
      const dtype = load<u8>(READDIR_BUF + offset + 20);

      const nameStart = offset + DIRENT_SIZE;
      if (nameStart + <i32>namelen > bufused) break; // partial entry

      // Read name bytes
      const name = String.UTF8.decodeUnsafe(READDIR_BUF + nameStart, namelen);

      // Skip . and ..
      if (name != "." && name != "..") {
        names.push(name);
        types.push(dtype);
      }

      offset = nameStart + <i32>namelen;
      cookie = nextCookie;
    }

    // If buffer was not full, we've read everything
    if (bufused < READDIR_BUF_LEN) break;
  }
}

// Simple insertion sort for parallel arrays
function sortEntries(names: string[], types: u8[]): void {
  for (let i = 1; i < names.length; i++) {
    const name = names[i];
    const dtype = types[i];
    let j = i - 1;
    while (j >= 0 && names[j] > name) {
      names[j + 1] = names[j];
      types[j + 1] = types[j];
      j--;
    }
    names[j + 1] = name;
    types[j + 1] = dtype;
  }
}

// List a directory recursively, printing prefix/name for each entry
function listDir(preopenFd: i32, relPath: string): void {
  let dirFd: i32;
  if (relPath.length == 0) {
    dirFd = preopenFd;
  } else {
    const pathLen = copyPath(PATH_BUF, relPath);
    dirFd = openDir(preopenFd, PATH_BUF, pathLen);
    if (dirFd < 0) return;
  }

  const names: string[] = [];
  const types: u8[] = [];
  readEntries(dirFd, names, types);
  sortEntries(names, types);

  if (relPath.length > 0) {
    fd_close(dirFd);
  }

  for (let i = 0; i < names.length; i++) {
    const fullPath = relPath.length > 0
      ? relPath + "/" + names[i]
      : names[i];

    writeStr(STDOUT, fullPath + "\n");

    if (types[i] == FILETYPE_DIRECTORY) {
      listDir(preopenFd, fullPath);
    }
  }
}

export function _start(): void {
  // Find preopen (fd 3)
  const err = fd_prestat_get(3, PRESTAT_BUF);
  if (err != ERRNO_SUCCESS) {
    writeStr(STDERR, "Error: no preopened directories (use --dir)\n");
    return;
  }

  // Check for optional subdir argument
  args_sizes_get(ARGC_PTR, ARGV_BUF_SIZE_PTR);
  const argc = load<i32>(ARGC_PTR);
  args_get(ARGV_PTR, ARGV_BUF_PTR);

  let startPath = "";
  if (argc >= 2) {
    const argPtr = load<i32>(ARGV_PTR + PTR_SIZE);
    let argLen: i32 = 0;
    while (load<u8>(argPtr + argLen) != 0) {
      argLen++;
    }
    startPath = String.UTF8.decodeUnsafe(argPtr, argLen);
  }

  listDir(3, startPath);
}
