#lang racket

(define (eval-left-to-right)
  (define f
    (let ((y 1))
      (lambda (x)
        (set! y (* x y))
        y)))
  (+ (f 0) (f 1)))

(define (eval-right-to-left)
  (define f
    (let ((y 1))
      (lambda (x)
        (set! y (* x y))
        y)))
  (+ (f 1) (f 0)))

;; TEST
(display "Left-to-right: ")
(display (eval-left-to-right))
(newline)
(display "Right-to-left: ")
(display (eval-right-to-left))
(newline)
