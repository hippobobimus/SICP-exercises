#lang sicp
(#%require "streams.scm")

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

;; Test
(define (integrate-series s)
  (stream-map *
              (stream-map / ones integers)
              s))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define cosine-series
  (cons-stream 1 (integrate-series (stream-map - sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define sin-2 (mul-series sine-series sine-series))
(define cos-2 (mul-series cosine-series cosine-series))
(display-stream-head (add-streams sin-2 cos-2))
