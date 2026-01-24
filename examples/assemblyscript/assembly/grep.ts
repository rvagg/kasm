// Grep - filters lines from stdin that contain a pattern

import {
  fd_read,
  fd_write,
  args_sizes_get,
  args_get,
  STDIN,
  STDOUT,
  STDERR,
  ERRNO_SUCCESS,
} from "./env";

// Memory layout:
// 0-7: iovec for I/O
// 8-11: nread/nwritten result
// 12-19: args sizes (argc + buf_size)
// 20-39: argv pointers (up to 5 args)
// 40-199: argv buffer
// 200-299: pattern storage (100 bytes max)
// 300-399: line buffer
// 400-4495: input buffer (4KB)

// iovec structure offsets
const IOVEC_BUF_PTR: i32 = 0;    // iovec.buf pointer (4 bytes)
const IOVEC_BUF_LEN: i32 = 4;    // iovec.buf_len (4 bytes)
const RESULT_PTR: i32 = 8;       // nread/nwritten result

// iovec array parameters
const IOVECS_PTR: i32 = 0;       // address of iovec array
const IOVECS_COUNT: i32 = 1;     // number of iovecs

// Argument handling
const ARGC_PTR: i32 = 12;        // argc result
const ARGV_BUF_SIZE_PTR: i32 = 16; // argv_buf_size result
const ARGV_PTR: i32 = 20;        // argv pointers array
const ARGV_BUF_PTR: i32 = 40;    // argv string buffer
const PTR_SIZE: i32 = 4;         // size of a pointer (i32)

// Data buffers
const LINE_PTR: i32 = 300;
const INPUT_PTR: i32 = 400;
const INPUT_SIZE: i32 = 4096;

// Current position in input buffer
let inputPos: i32 = 0;
let inputLen: i32 = 0;
let inputEof: bool = false;

// Pattern info
let patternPtr: i32 = 0;
let patternLen: i32 = 0;

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

function refillBuffer(): bool {
  if (inputEof) return false;

  store<i32>(IOVEC_BUF_PTR, INPUT_PTR);
  store<i32>(IOVEC_BUF_LEN, INPUT_SIZE);

  const errno = fd_read(STDIN, IOVECS_PTR, IOVECS_COUNT, RESULT_PTR);
  if (errno != ERRNO_SUCCESS) {
    inputEof = true;
    return false;
  }

  inputLen = load<i32>(RESULT_PTR);
  inputPos = 0;

  if (inputLen == 0) {
    inputEof = true;
    return false;
  }

  return true;
}

// Read a line into LINE_PTR, returns length or -1 for EOF
function readLine(): i32 {
  let lineLen: i32 = 0;
  const maxLen: i32 = 99; // Leave room for newline

  while (lineLen < maxLen) {
    // Refill if needed
    if (inputPos >= inputLen) {
      if (!refillBuffer()) {
        // EOF - return what we have
        if (lineLen > 0) {
          return lineLen;
        }
        return -1;
      }
    }

    const c = load<u8>(INPUT_PTR + inputPos);
    inputPos++;

    if (c == 0x0a) { // '\n'
      return lineLen;
    }

    store<u8>(LINE_PTR + lineLen, c);
    lineLen++;
  }

  return lineLen;
}

// Check if line contains pattern (simple substring search)
function lineContainsPattern(lineLen: i32): bool {
  if (patternLen == 0) return true;
  if (lineLen < patternLen) return false;

  const searchEnd = lineLen - patternLen + 1;
  for (let i: i32 = 0; i < searchEnd; i++) {
    let match = true;
    for (let j: i32 = 0; j < patternLen; j++) {
      if (load<u8>(LINE_PTR + i + j) != load<u8>(patternPtr + j)) {
        match = false;
        break;
      }
    }
    if (match) return true;
  }
  return false;
}

export function _start(): void {
  // Get args
  const errno1 = args_sizes_get(ARGC_PTR, ARGV_BUF_SIZE_PTR);
  if (errno1 != ERRNO_SUCCESS) {
    writeStderr("Error: failed to get args sizes\n");
    return;
  }

  const argc = load<i32>(ARGC_PTR);
  if (argc < 2) {
    writeStderr("Usage: grep <pattern>\n");
    return;
  }

  const errno2 = args_get(ARGV_PTR, ARGV_BUF_PTR);
  if (errno2 != ERRNO_SUCCESS) {
    writeStderr("Error: failed to get args\n");
    return;
  }

  // Get pattern from argv[1]
  patternPtr = load<i32>(ARGV_PTR + PTR_SIZE);
  patternLen = 0;
  while (load<u8>(patternPtr + patternLen) != 0) {
    patternLen++;
  }

  // Read lines and filter
  while (true) {
    const lineLen = readLine();
    if (lineLen < 0) break;

    if (lineContainsPattern(lineLen)) {
      writeStdout(LINE_PTR, lineLen);
      // Write newline
      store<u8>(LINE_PTR + lineLen, 0x0a);
      writeStdout(LINE_PTR + lineLen, 1);
    }
  }
}
