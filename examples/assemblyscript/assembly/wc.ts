// wc - count lines, words, and bytes in a file
//
// Uses fd_seek to determine file size, then reads and counts.
// Usage: kasm run --dir=. wc.wasm -- filename.txt

import {
  fd_read,
  fd_write,
  fd_close,
  fd_seek,
  args_sizes_get,
  args_get,
  fd_prestat_get,
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
const SEEK_RESULT_PTR: i32 = BASE + 310;

// Read buffer: 2KB at BASE + 512
const DATA_BUF: i32 = BASE + 512;
const DATA_SIZE: i32 = 2048;

function writeStr(fd: i32, msg: string): void {
  const encoded = String.UTF8.encode(msg);
  store<i32>(IOVEC_BUF_PTR, changetype<i32>(encoded));
  store<i32>(IOVEC_BUF_LEN, encoded.byteLength);
  fd_write(fd, IOVECS_PTR, IOVECS_COUNT, RESULT_PTR);
}

export function _start(): void {
  // Get args
  args_sizes_get(ARGC_PTR, ARGV_BUF_SIZE_PTR);
  const argc = load<i32>(ARGC_PTR);
  args_get(ARGV_PTR, ARGV_BUF_PTR);

  if (argc < 2) {
    writeStr(STDERR, "Usage: wc <filename>\n");
    return;
  }

  // Get filename from argv[1]
  const filenamePtr = load<i32>(ARGV_PTR + PTR_SIZE);
  let filenameLen: i32 = 0;
  while (load<u8>(filenamePtr + filenameLen) != 0) {
    filenameLen++;
  }

  // Find preopen
  const err = fd_prestat_get(3, PRESTAT_BUF);
  if (err != ERRNO_SUCCESS) {
    writeStr(STDERR, "Error: no preopened directories (use --dir)\n");
    return;
  }

  // Open the file
  const openErr = path_open(
    3, 0, filenamePtr, filenameLen,
    0, <i64>0x1fffffff, <i64>0x1fffffff, 0,
    OPENED_FD_PTR
  );
  if (openErr != ERRNO_SUCCESS) {
    writeStr(STDERR, "Error: could not open file (errno " + openErr.toString() + ")\n");
    return;
  }

  const fileFd = load<i32>(OPENED_FD_PTR);

  // Use fd_seek to get file size: seek to end (whence=2, offset=0)
  const seekErr = fd_seek(fileFd, 0, 2, SEEK_RESULT_PTR);
  if (seekErr != ERRNO_SUCCESS) {
    writeStr(STDERR, "Error: seek failed\n");
    fd_close(fileFd);
    return;
  }
  const fileSize = load<i32>(SEEK_RESULT_PTR); // low 32 bits of u64

  // Seek back to start (whence=0, offset=0)
  fd_seek(fileFd, 0, 0, SEEK_RESULT_PTR);

  // Read and count
  let lines: i32 = 0;
  let words: i32 = 0;
  let bytes: i32 = 0;
  let inWord: bool = false;

  while (true) {
    store<i32>(IOVEC_BUF_PTR, DATA_BUF);
    store<i32>(IOVEC_BUF_LEN, DATA_SIZE);
    const readErr = fd_read(fileFd, IOVECS_PTR, IOVECS_COUNT, RESULT_PTR);
    if (readErr != ERRNO_SUCCESS) break;
    const nread = load<i32>(RESULT_PTR);
    if (nread == 0) break;

    for (let i: i32 = 0; i < nread; i++) {
      const ch = load<u8>(DATA_BUF + i);
      bytes++;

      if (ch == 10) { // newline
        lines++;
        inWord = false;
      } else if (ch == 32 || ch == 9 || ch == 13) { // space, tab, CR
        inWord = false;
      } else {
        if (!inWord) {
          words++;
          inWord = true;
        }
      }
    }
  }

  fd_close(fileFd);

  // Output: "  lines  words  bytes filename\n"
  const output = "  " + lines.toString() + "  " + words.toString() + "  " + bytes.toString() + " (size: " + fileSize.toString() + ")\n";
  writeStr(STDOUT, output);
}
