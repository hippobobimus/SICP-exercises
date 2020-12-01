#lang sicp
(#%require "streams.scm")

;; Part (a)
(define (integrate-series s)
  (stream-map *
              (stream-map / ones integers)
              s))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(display-stream-head ones)
(display-stream-head (integrate-series ones))

;; Part (b)
(define cosine-series
  (cons-stream 1 (integrate-series (stream-map - sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(display-stream-head cosine-series)
(display-stream-head sine-series)
