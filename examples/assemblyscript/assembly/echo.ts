// Echo - reads stdin and writes to stdout

import { fd_read, fd_write, STDIN, STDOUT, ERRNO_SUCCESS } from "./env";

// Memory layout constants
const IOVEC_BUF_PTR: i32 = 0;    // iovec.buf pointer (4 bytes)
const IOVEC_BUF_LEN: i32 = 4;    // iovec.buf_len (4 bytes)
const RESULT_PTR: i32 = 8;       // nread/nwritten result (4 bytes)
const BUFFER_PTR: i32 = 16;      // data buffer start
const BUFFER_SIZE: i32 = 4096;   // data buffer size

// iovec array parameters
const IOVECS_PTR: i32 = 0;       // address of iovec array
const IOVECS_COUNT: i32 = 1;     // number of iovecs

export function _start(): void {
  // Set up iovec for reading
  store<i32>(IOVEC_BUF_PTR, BUFFER_PTR);
  store<i32>(IOVEC_BUF_LEN, BUFFER_SIZE);

  while (true) {
    // Read from stdin
    const errno = fd_read(STDIN, IOVECS_PTR, IOVECS_COUNT, RESULT_PTR);
    if (errno != ERRNO_SUCCESS) {
      break;
    }

    const bytesRead = load<i32>(RESULT_PTR);
    if (bytesRead == 0) {
      break; // EOF
    }

    // Update iovec length to bytes actually read
    store<i32>(IOVEC_BUF_LEN, bytesRead);

    // Write to stdout
    fd_write(STDOUT, IOVECS_PTR, IOVECS_COUNT, RESULT_PTR);
  }
}
