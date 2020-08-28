
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (rep-iter g n)
    (cond ((= n 1) g)
          ((even? n) (rep-iter (compose g g) (/ n 2)))
          (else (rep-iter (compose f g) (- n 1)))))
  (rep-iter f n))

(define (square-root x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (n-root x n)
  (fixed-point ((repeated average-damp (floor (log n))) (lambda (y) (/ x (expt y (- n 1)))))
               1.0))
