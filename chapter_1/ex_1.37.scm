(define (cont-frac n d k) ; iterative
  (define (cont-frac-iter x result)
    (if (= x 0)
        result
        (cont-frac-iter (- x 1) (/ (n x) (+ (d x) result)))))
  (cont-frac-iter k 0))

(define (cont-frac-rec n d k) ; recursive
  (define (rec x)
    (if (> x k)
        0
        (/ (n k) (+ (d k) (rec (+ x 1))))))
  (rec 1.0))

(define (inverse-phi k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))

(define (inverse-phi-rec k)
  (cont-frac-rec (lambda (i) 1.0)
                 (lambda (i) 1.0)
                 k))
