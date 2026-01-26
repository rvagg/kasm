;; primes.wat - Sieve of Eratosthenes
;;
;; Counts prime numbers up to $limit using the classic sieve algorithm.
;; Uses memory as a bitmap (1 byte per number for simplicity).
;;
;; Expected values (verifiable):
;;   count_primes(10)     = 4     (2, 3, 5, 7)
;;   count_primes(100)    = 25
;;   count_primes(1000)   = 168
;;   count_primes(10000)  = 1229
;;   count_primes(100000) = 9592
;;
;; Memory layout:
;;   0 to limit-1: sieve array (0 = prime candidate, 1 = composite)
(module
  (memory (export "memory") 2)  ;; 2 pages = 128KB, supports limit up to ~128000

  (func (export "count_primes") (param $limit i32) (result i32)
    (local $i i32)
    (local $j i32)
    (local $count i32)

    ;; Initialise: mark 0 and 1 as non-prime
    (i32.store8 (i32.const 0) (i32.const 1))
    (i32.store8 (i32.const 1) (i32.const 1))

    ;; Clear rest of sieve (2 to limit-1)
    (local.set $i (i32.const 2))
    (block $init_done
      (loop $init_loop
        (br_if $init_done (i32.ge_u (local.get $i) (local.get $limit)))
        (i32.store8 (local.get $i) (i32.const 0))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $init_loop)
      )
    )

    ;; Sieve: for each prime p, mark multiples as composite
    (local.set $i (i32.const 2))
    (block $sieve_done
      (loop $sieve_loop
        ;; if i*i >= limit, we're done sieving
        (br_if $sieve_done
          (i32.ge_u (i32.mul (local.get $i) (local.get $i)) (local.get $limit)))

        ;; if sieve[i] == 0, i is prime; mark its multiples
        (if (i32.eqz (i32.load8_u (local.get $i)))
          (then
            ;; j = i*i (smaller multiples already marked by smaller primes)
            (local.set $j (i32.mul (local.get $i) (local.get $i)))
            (block $mark_done
              (loop $mark_loop
                (br_if $mark_done (i32.ge_u (local.get $j) (local.get $limit)))
                ;; sieve[j] = 1 (composite)
                (i32.store8 (local.get $j) (i32.const 1))
                ;; j += i
                (local.set $j (i32.add (local.get $j) (local.get $i)))
                (br $mark_loop)
              )
            )
          )
        )

        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $sieve_loop)
      )
    )

    ;; Count primes
    (local.set $count (i32.const 0))
    (local.set $i (i32.const 2))
    (block $count_done
      (loop $count_loop
        (br_if $count_done (i32.ge_u (local.get $i) (local.get $limit)))

        ;; if sieve[i] == 0, it's prime
        (if (i32.eqz (i32.load8_u (local.get $i)))
          (then
            (local.set $count (i32.add (local.get $count) (i32.const 1)))
          )
        )

        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $count_loop)
      )
    )

    (local.get $count)
  )
)
