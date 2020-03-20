(define (even? n)
    (= (remainder n 2) 0))

(define (new-expt b n)
    (fast-expt-iter b n 1))

(define (fast-expt-iter b counter a)
    (cond ((= counter 0) 1)
          ((= counter 1) (* b a))
          ((even? counter) (fast-expt-iter (square b) (/ counter 2) a))
          (else (fast-expt-iter b (- counter 1) (* b a)))))

