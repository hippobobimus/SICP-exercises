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
  (try-it (+ 1 (random (- n 1)))))

(define (mr-prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (mr-prime? n (- times 1)))
        (else #f)))

(define (prime? n)
  (mr-prime? n 100))

