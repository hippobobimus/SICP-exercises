
(define (cont-frac n d k) ; iterative
  (define (cont-frac-iter x result)
    (if (= x 0)
        result
        (cont-frac-iter (- x 1) (/ (n x) (+ (d x) result)))))
  (cont-frac-iter k 0))


(define (tan-cf x k)
  (let ((y (-(square x))))
    (cont-frac (lambda (i) (if (= i 1) x y))
               (lambda (i) (- (* 2 i) 1))
               k)))
