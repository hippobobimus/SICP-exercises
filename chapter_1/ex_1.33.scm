
(define (filtered-accumulate-recursive combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter? a) (combiner (term a)
                               (filtered-accumulate-recursive filter? combiner null-value term (next a) next b)))
        (else (filtered-accumulate-recursive filter? combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

; iterative version
(define (filtered-accumulate filter? combiner null-value term a next b)
  (define (acc-iter a result)
    (cond ((> a b) result)
          ((filter? a) (acc-iter (next a) (combiner result (term a))))
          (else (acc-iter (next a) result))))
  (acc-iter a null-value))


; prime checker from ex 1.28.
(define (non-trivial-sqrt? a n)
  (if (or (= a (- n 1)) (= a 1))
      #f
      (= (remainder (square a) n) 1)))

(define (expmod a n m)
  (cond ((= n 0) 1)
        ((even? n)
         (let ((x (expmod a (/ n 2) m)))
             (if (non-trivial-sqrt? x m)
                 0
                 (remainder (square x) m))))
        (else
          (remainder (* a (expmod a (- n 1) m))
                     m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (if (and (> n 0) (< n 4))
      true
      (try-it (+ 1 (random (- n 1))))))

(define (mr-prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (mr-prime? n (- times 1)))
        (else #f)))

(define (prime? n)
  (mr-prime? n 100))

; Sum of the squares of the prime numbers in the interval a to b.
(define (sum-of-squared-primes a b)
  (define (sosp-term x)
    (square x))
  (define (sosp-next x)
    (+ x 1))
  (filtered-accumulate prime? + 0 sosp-term a sosp-next b))


; Sum of all the positive integers less than n that are relatively prime to n.
; i.e. i < n s.t. GCD(i, n) = 1.
(define (GCD a b)
  (if (= b 0)
      a
      (GCD b (remainder a b))))

(define (product-of-rel-primes n)
  (define (relative-prime? x)
    (= (GCD n x) 1))
  (define (porp-term x)
    x)
  (define (porp-next x)
    (+ x 1))
  (filtered-accumulate relative-prime? * 1 porp-term 1 porp-next (- n 1)))
