;; memcpy.wat - Byte-by-byte memory copy
;;
;; Copies $len bytes from $src to $dst.
;; Returns the number of bytes copied.
;;
;; This is intentionally naive (byte-by-byte) to measure individual
;; memory operation overhead. A real memcpy would use wider loads.
;;
;; Memory layout for testing:
;;   0-4095:     source buffer (fill with test pattern)
;;   4096-8191:  destination buffer
;;
;; Expected: copy(0, 4096, 1000) copies 1000 bytes, returns 1000
(module
  (memory (export "memory") 1)  ;; 1 page = 64KB

  (func (export "copy") (param $src i32) (param $dst i32) (param $len i32) (result i32)
    (local $i i32)
    (local $byte i32)

    (block $done
      (loop $loop
        ;; if i >= len, exit
        (br_if $done (i32.ge_u (local.get $i) (local.get $len)))

        ;; byte = mem[src + i]
        (local.set $byte
          (i32.load8_u (i32.add (local.get $src) (local.get $i))))

        ;; mem[dst + i] = byte
        (i32.store8
          (i32.add (local.get $dst) (local.get $i))
          (local.get $byte))

        ;; i++
        (local.set $i (i32.add (local.get $i) (i32.const 1)))

        (br $loop)
      )
    )

    (local.get $i)
  )

  ;; Fill memory with a pattern for testing
  ;; fill(ptr, len, value) - fills len bytes starting at ptr with value
  (func (export "fill") (param $ptr i32) (param $len i32) (param $value i32) (result i32)
    (local $i i32)

    (block $done
      (loop $loop
        (br_if $done (i32.ge_u (local.get $i) (local.get $len)))

        (i32.store8
          (i32.add (local.get $ptr) (local.get $i))
          (local.get $value))

        (local.set $i (i32.add (local.get $i) (i32.const 1)))

        (br $loop)
      )
    )

    (local.get $i)
  )

  ;; Verify copy by checking all bytes match
  ;; Returns 1 if all bytes match, 0 otherwise
  (func (export "verify") (param $src i32) (param $dst i32) (param $len i32) (result i32)
    (local $i i32)

    (block $done
      (loop $loop
        (br_if $done (i32.ge_u (local.get $i) (local.get $len)))

        ;; if mem[src+i] != mem[dst+i], return 0
        (if (i32.ne
              (i32.load8_u (i32.add (local.get $src) (local.get $i)))
              (i32.load8_u (i32.add (local.get $dst) (local.get $i))))
          (then (return (i32.const 0)))
        )

        (local.set $i (i32.add (local.get $i) (i32.const 1)))

        (br $loop)
      )
    )

    (i32.const 1)
  )
)
