#lang sicp
(#%require "streams.scm")

(define (stream-limit s n)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1)))
    (if (< (abs (- s0 s1)) n)
        s1
        (stream-limit (stream-cdr s) n))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;; Test
(display "Sqrt stream: ")
(display-stream-head (sqrt-stream 12.0))
(display "sqrt: ")
(display (sqrt 12.0 0.1))
(newline)
