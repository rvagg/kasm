// mktree - create a known directory tree structure for testing
//
// Creates directories, files, renames, and deletes to exercise filesystem
// mutation WASI APIs. Prints each operation and result to stdout.
//
// Usage: kasm run --dir=/tmp/testdir mktree.wasm
//
// Expected output (on success):
//   mkdir sub: 0
//   mkdir sub/deep: 0
//   create sub/hello.txt: 0
//   write sub/hello.txt: 0
//   create sub/deep/nested.txt: 0
//   write sub/deep/nested.txt: 0
//   create sub/remove-me.txt: 0
//   unlink sub/remove-me.txt: 0
//   rename sub/hello.txt -> sub/greeting.txt: 0
//   mkdir sub/empty-dir: 0
//   done

import {
  fd_write,
  fd_close,
  fd_prestat_get,
  path_open,
  path_create_directory,
  path_remove_directory,
  path_unlink_file,
  path_rename,
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

const PRESTAT_BUF: i32 = BASE + 200;
const OPENED_FD_PTR: i32 = BASE + 300;

// Scratch area for raw path bytes (not AS strings)
const PATH_BUF: i32 = BASE + 400;
const PATH_BUF2: i32 = BASE + 600;

// Write buffer for file contents
const WRITE_BUF: i32 = BASE + 800;

// Write a string to a file descriptor via fd_write. The encoded ArrayBuffer
// lives on the AS heap which is part of linear memory, so its pointer is
// valid for fd_write to read from directly (no copy to scratch needed).
function writeStr(fd: i32, msg: string): void {
  const encoded = String.UTF8.encode(msg);
  store<i32>(IOVEC_BUF_PTR, changetype<i32>(encoded));
  store<i32>(IOVEC_BUF_LEN, encoded.byteLength);
  fd_write(fd, IOVECS_PTR, IOVECS_COUNT, RESULT_PTR);
}

// Encode a string into raw memory at dest, return byte length
function copyStr(dest: i32, s: string): i32 {
  const encoded = String.UTF8.encode(s);
  const len = encoded.byteLength;
  memory.copy(dest, changetype<i32>(encoded), len);
  return len;
}

function createAndWriteFile(preopenFd: i32, path: string, content: string): void {
  const pathLen = copyStr(PATH_BUF, path);

  // O_CREAT | O_TRUNC = 1 | 8 = 9
  const err = path_open(
    preopenFd, 0, PATH_BUF, pathLen,
    9, <i64>0x1fffffff, <i64>0x1fffffff, 0,
    OPENED_FD_PTR
  );
  writeStr(STDOUT, "create " + path + ": " + err.toString() + "\n");
  if (err != ERRNO_SUCCESS) return;

  const fd = load<i32>(OPENED_FD_PTR);
  const dataLen = copyStr(WRITE_BUF, content);
  store<i32>(IOVEC_BUF_PTR, WRITE_BUF);
  store<i32>(IOVEC_BUF_LEN, dataLen);
  const writeErr = fd_write(fd, IOVECS_PTR, IOVECS_COUNT, RESULT_PTR);
  writeStr(STDOUT, "write " + path + ": " + writeErr.toString() + "\n");
  fd_close(fd);
}

function mkdir(preopenFd: i32, path: string): i32 {
  const pathLen = copyStr(PATH_BUF, path);
  const err = path_create_directory(preopenFd, PATH_BUF, pathLen);
  writeStr(STDOUT, "mkdir " + path + ": " + err.toString() + "\n");
  return err;
}

function rename(preopenFd: i32, oldPath: string, newPath: string): void {
  const oldLen = copyStr(PATH_BUF, oldPath);
  const newLen = copyStr(PATH_BUF2, newPath);
  const err = path_rename(preopenFd, PATH_BUF, oldLen, preopenFd, PATH_BUF2, newLen);
  writeStr(STDOUT, "rename " + oldPath + " -> " + newPath + ": " + err.toString() + "\n");
}

function unlink(preopenFd: i32, path: string): void {
  const pathLen = copyStr(PATH_BUF, path);
  const err = path_unlink_file(preopenFd, PATH_BUF, pathLen);
  writeStr(STDOUT, "unlink " + path + ": " + err.toString() + "\n");
}

export function _start(): void {
  // Find preopen (fd 3)
  const err = fd_prestat_get(3, PRESTAT_BUF);
  if (err != ERRNO_SUCCESS) {
    writeStr(STDERR, "Error: no preopened directories (use --dir)\n");
    return;
  }
  const preopenFd: i32 = 3;

  // Create directory structure
  mkdir(preopenFd, "sub");
  mkdir(preopenFd, "sub/deep");

  // Create files with known content
  createAndWriteFile(preopenFd, "sub/hello.txt", "Hello, world!\n");
  createAndWriteFile(preopenFd, "sub/deep/nested.txt", "Deep content\n");

  // Create a file then delete it
  createAndWriteFile(preopenFd, "sub/remove-me.txt", "temporary\n");
  unlink(preopenFd, "sub/remove-me.txt");

  // Rename a file
  rename(preopenFd, "sub/hello.txt", "sub/greeting.txt");

  // Create an empty directory
  mkdir(preopenFd, "sub/empty-dir");

  writeStr(STDOUT, "done\n");
}
