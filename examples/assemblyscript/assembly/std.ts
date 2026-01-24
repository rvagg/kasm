// Standard library wrappers for WASI functions

import { fd_write, STDOUT, STDERR } from "./env";

// Memory layout constants
const IOVEC_BUF_PTR: i32 = 0;    // iovec.buf pointer (4 bytes)
const IOVEC_BUF_LEN: i32 = 4;    // iovec.buf_len (4 bytes)
const RESULT_PTR: i32 = 8;       // nwritten result (4 bytes)

// iovec array parameters
const IOVECS_PTR: i32 = 0;       // address of iovec array
const IOVECS_COUNT: i32 = 1;     // number of iovecs

/**
 * Print a string to stdout
 */
export function print(s: string): void {
  const encoded = String.UTF8.encode(s);
  const ptr = changetype<i32>(encoded);
  const len = encoded.byteLength;

  // Set up iovec: {ptr, len}
  store<i32>(IOVEC_BUF_PTR, ptr);
  store<i32>(IOVEC_BUF_LEN, len);

  // Call fd_write
  fd_write(STDOUT, IOVECS_PTR, IOVECS_COUNT, RESULT_PTR);
}

/**
 * Print a string followed by newline
 */
export function println(s: string): void {
  print(s);
  print("\n");
}

/**
 * Print to stderr
 */
export function eprint(s: string): void {
  const encoded = String.UTF8.encode(s);
  const ptr = changetype<i32>(encoded);
  const len = encoded.byteLength;

  store<i32>(IOVEC_BUF_PTR, ptr);
  store<i32>(IOVEC_BUF_LEN, len);

  fd_write(STDERR, IOVECS_PTR, IOVECS_COUNT, RESULT_PTR);
}

/**
 * Print to stderr followed by newline
 */
export function eprintln(s: string): void {
  eprint(s);
  eprint("\n");
}
