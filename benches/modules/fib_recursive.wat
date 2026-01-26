;; fib_recursive.wat - Recursive Fibonacci
;;
;; Computes the nth Fibonacci number recursively.
;; This is intentionally inefficient (exponential time) to stress-test
;; function call overhead. For n=30, makes ~2.7 million calls.
;;
;; Expected values (same as iterative):
;;   fib(0)  = 0
;;   fib(1)  = 1
;;   fib(10) = 55
;;   fib(20) = 6765
;;   fib(30) = 832040
(module
  (func $fib (export "fib") (param $n i32) (result i32)
    ;; Base cases: fib(0) = 0, fib(1) = 1
    (if (result i32) (i32.le_u (local.get $n) (i32.const 1))
      (then (local.get $n))
      (else
        ;; fib(n) = fib(n-1) + fib(n-2)
        (i32.add
          (call $fib (i32.sub (local.get $n) (i32.const 1)))
          (call $fib (i32.sub (local.get $n) (i32.const 2)))
        )
      )
    )
  )
)
