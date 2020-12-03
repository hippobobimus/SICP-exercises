#lang sicp
(#%require "streams.scm")

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map (lambda (x) (cons (stream-car s) x))
                  (stream-cdr (pairs t u)))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams integers ones)))
(define s (pairs integers integers))
(define t (triples integers integers integers))

(display-stream-head s)
(display-stream-head t)

(define (pythagorean-triples s)
  (stream-filter (lambda (triple)
                   (= (+ (square (car triple))
                         (square (cadr triple)))
                      (square (caddr triple))))
                 (triples s s s)))

(define (square x) (* x x))

(display-stream-head-n (pythagorean-triples integers) 5)
