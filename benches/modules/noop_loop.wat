;; noop_loop.wat - Measures pure instruction dispatch overhead
;;
;; Executes n iterations of a minimal loop (increment + branch).
;; Returns the final counter value, which should equal the input n.
;;
;; Expected: run(1000) = 1000
(module
  (func (export "run") (param $n i32) (result i32)
    (local $i i32)
    ;; $i starts at 0 (default)
    (block $done
      (loop $loop
        ;; if $i >= $n, exit
        (br_if $done (i32.ge_u (local.get $i) (local.get $n)))
        ;; $i = $i + 1
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        ;; continue loop
        (br $loop)
      )
    )
    (local.get $i)
  )
)
