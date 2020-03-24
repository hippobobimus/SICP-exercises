
(define (cont-frac n d k) ; iterative
  (define (cont-frac-iter x result)
    (if (= x 0)
        result
        (cont-frac-iter (- x 1) (/ (n x) (+ (d x) result)))))
  (cont-frac-iter k 0))


(define (euler-cf k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 
               (if (= (remainder (+ i 1) 3) 0)
                   (* (/ 2 3) (+ i 1))
                   1))
             k))

(define (compute-e k)
  (+ (euler-cf k) 2))
