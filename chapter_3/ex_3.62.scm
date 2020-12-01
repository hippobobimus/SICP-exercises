#lang sicp
(#%require "streams.scm")

(define (integrate-series s)
  (stream-map *
              (stream-map / ones integers)
              s))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

(define (invert-unit-series s)
  (cons-stream 1 (mul-series (stream-map - (stream-cdr s))
                             (invert-unit-series s))))

(define (div-series numer denom)
  (if (= (stream-car denom) 0)
      (error "Denominator series must begins with a non-zero constant term -- DIV-SERIES"
             denom)
      (mul-series numer
                  (invert-unit-series denom))))

;; Test
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams integers ones)))
(display-stream-head (div-series ones integers))

(define cosine-series
  (cons-stream 1 (integrate-series (stream-map - sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define tangent-series
  (div-series sine-series cosine-series))

(display-stream-head tangent-series)  ;; See 'Taylor series'.
