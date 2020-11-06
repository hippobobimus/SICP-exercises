#lang racket

(define (make-accumulator n)
  (let ((init-val n))
    (lambda (x)
      (set! init-val (+ init-val x))
      init-val)))

;; TEST
(define A (make-accumulator 5))
(display "(A 10): ")
(display (A 10))
(newline)
(display "(A 10): ")
(display (A 10))
(newline)
