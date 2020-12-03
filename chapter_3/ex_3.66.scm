#lang sicp
(#%require "streams.scm")

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams integers ones)))
(define s (pairs integers integers))

(define (formula-test i j)
  (define (formula c)
    (- (* (+ (* 2 (- j i))
             1)
          (expt 2 c))
       2))
  (define (get-index)
    (cond ((= i j) (formula i))
          ((< i j) (formula (- i 1)))
          (else 
            (error "i must be less than or equal to j" (list i j)))))
  (let ((index (get-index)))
    (display "Finding (")
    (display i)
    (display " ")
    (display j)
    (display ")")
    (newline)
    (display "Formula index prediction: ")
    (display index)
    (newline)
    (display "Element with this index is: ")
    (display (stream-ref s index))
    (newline)))

(formula-test 4 9)
(formula-test 7 21)
(formula-test 14 18)
(formula-test 9 9)


