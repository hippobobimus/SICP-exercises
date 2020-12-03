#lang sicp
(#%require "streams.scm")
(#%require racket/trace)

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(trace-define (pairs-louis s t)
  (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs-louis (stream-cdr s) (stream-cdr t))))

(trace-define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams integers ones)))
(define s (pairs integers integers))
(define s-louis (pairs-louis integers integers))

(display-stream-head s)
(display-stream-head s-louis)

