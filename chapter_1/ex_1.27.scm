
(define (expmod base expn m)
  (cond ((= expn 0) 1)
        ((even? expn)
         (remainder (square (expmod base (/ expn 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- expn 1) m))
                     m))))

(define (fermat-test a n)
  (= (expmod a n n) a))

(define (carm-test-iter n counter)
  (cond ((= counter n)
         #t)
        ((fermat-test counter n)
         (carm-test-iter n (+ counter 1)))
        (else
          #f)))

(define (carm-test n)
  (carm-test-iter n 1))
