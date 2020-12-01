#lang sicp
(#%require "streams.scm")

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define integers (cons-stream 1 (add-streams integers ones)))

(define ones (cons-stream 1 ones))

;; Exercise
(define factorials (cons-stream 1 (mul-streams (stream-cdr integers) factorials)))

(display-stream-head factorials)
