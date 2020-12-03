#lang sicp
(#%require "streams.scm")

(define (pairs-old s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs-old (stream-cdr s) (stream-cdr t)))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (interleave
        (stream-map (lambda (x) (list (stream-car s) x))
                    (stream-cdr t))
        (stream-map (lambda (x) (list x (stream-car t)))
                    (stream-cdr s)))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams integers ones)))
(define s (pairs integers integers))
(define s-old (pairs-old integers integers))

(display-stream-head s-old)
(display-stream-head s)
