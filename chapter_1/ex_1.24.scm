(define (expmod base expn m)
  (cond ((= expn 0) 1)
        ((even? expn)
         (remainder (square (expmod base (/ expn 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- expn 1) m))
                     m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (prime? n)
  (fast-prime? n 100))


(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  #t)



(define (search-for-primes start)
  (if (even? start)
      (search-for-primes-iter (+ start 1) 3 0)
      (search-for-primes-iter start 3 0)))

(define (search-for-primes-iter n no-of-primes counter)
  (cond ((= counter no-of-primes)
         (begin (newline)
                (display "SEARCH COMPLETE")))
        ((timed-prime-test n)
         (search-for-primes-iter (+ n 2) no-of-primes (+ counter 1)))
        (else
         (search-for-primes-iter (+ n 2) no-of-primes counter))))

