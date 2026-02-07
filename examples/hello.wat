(module
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))

  (memory (export "memory") 1)

  ;; iov_base at offset 0, pointing to string at offset 8
  (data (i32.const 0) "\08\00\00\00")
  ;; iov_len at offset 4
  (data (i32.const 4) "\0e\00\00\00")
  ;; string at offset 8
  (data (i32.const 8) "Hello, world!\n")

  (func (export "_start")
    ;; fd_write(stdout=1, iovs=0, iovs_len=1, nwritten=100)
    (drop (call $fd_write
      (i32.const 1)
      (i32.const 0)
      (i32.const 1)
      (i32.const 100)))))
