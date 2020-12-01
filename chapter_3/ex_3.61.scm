#lang sicp
(#%require "streams.scm")

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

(define (invert-unit-series s)
  (cons-stream 1 (mul-series (stream-map - (stream-cdr s))
                             (invert-unit-series s))))

;; Test
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(display-stream-head integers)
(display-stream-head (invert-unit-series integers))
(display-stream-head (mul-series (invert-unit-series integers)
                                 integers))
