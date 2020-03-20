(define (halve x)
    (/ x 2))

(define (double x)
    (+ x x))

(define (fast-mult x y)
    (define (fast-mult-iter a b r)
        (cond ((or (= a 0) (= b 0)) 0)
              ((= b 1) (+ a r))
              ((= b (- 1)) (+ (- a) r))
              ((even? b) (fast-mult-iter (double a) (halve b) r))
              (else (fast-mult-iter (+ a 1) (- b 1) (+ r a (- b) 1)))))
    (fast-mult-iter x y 0))
