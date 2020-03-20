
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        ((= test-divisor 2) (find-divisor n 3))
        (else (find-divisor n (+ test-divisor 2)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))



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

