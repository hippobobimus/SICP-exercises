(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (rep-iter g n)
    (cond ((= n 1) g)
          ((even? n) (rep-iter (compose g g) (/ n 2)))
          (else (rep-iter (compose f g) (- n 1)))))
  (rep-iter f n))
