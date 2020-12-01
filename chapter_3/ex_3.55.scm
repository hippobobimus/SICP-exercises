#lang sicp
(#%require "streams.scm")

(define integers (cons-stream 1 (add-streams integers ones)))
(define ones (cons-stream 1 ones))

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s)
                            (stream-cdr s))))

(display-stream-head (partial-sums integers))
