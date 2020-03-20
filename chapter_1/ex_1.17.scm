(define (halve x)
    (/ x 2))

(define (double x)
    (+ x x))

(define (fast-mult a b)
    (cond ((or (= a 0) (= b 0)) 0)
          ((even? b) (double (fast-mult a (halve b))))
          ((< b 0) (+ (- a) (fast-mult a (+ b 1))))
          (else (+ a (fast-mult a (- b 1))))))
