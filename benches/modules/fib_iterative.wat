;; fib_iterative.wat - Iterative Fibonacci
;;
;; Computes the nth Fibonacci number iteratively.
;; Uses the standard algorithm: fib(0)=0, fib(1)=1, fib(n)=fib(n-1)+fib(n-2)
;;
;; Expected values (verifiable):
;;   fib(0)  = 0
;;   fib(1)  = 1
;;   fib(10) = 55
;;   fib(20) = 6765
;;   fib(40) = 102334155
;;   fib(46) = 1836311903 (max before i32 overflow)
(module
  (func (export "fib") (param $n i32) (result i32)
    (local $a i32)  ;; fib(i-2)
    (local $b i32)  ;; fib(i-1)
    (local $tmp i32)
    (local $i i32)

    ;; Handle n=0 case
    (if (i32.eqz (local.get $n))
      (then (return (i32.const 0)))
    )

    ;; Initialise: a=0 (fib(0)), b=1 (fib(1))
    (local.set $a (i32.const 0))
    (local.set $b (i32.const 1))
    (local.set $i (i32.const 1))

    ;; Loop from i=1 to n-1, computing fib(i+1) each iteration
    (block $done
      (loop $loop
        ;; if i >= n, we're done
        (br_if $done (i32.ge_u (local.get $i) (local.get $n)))

        ;; tmp = a + b (next fib number)
        (local.set $tmp (i32.add (local.get $a) (local.get $b)))
        ;; a = b
        (local.set $a (local.get $b))
        ;; b = tmp
        (local.set $b (local.get $tmp))
        ;; i++
        (local.set $i (i32.add (local.get $i) (i32.const 1)))

        (br $loop)
      )
    )

    ;; Return b, which holds fib(n)
    (local.get $b)
  )
)
