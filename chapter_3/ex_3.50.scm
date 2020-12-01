#lang sicp
(#%require "streams.scm")

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;; TEST
(define ints (cons-stream 1 (stream-map (lambda (x) (+ x 1)) ints)))

(display (stream-car ints))
(newline)
(display (stream-car (stream-cdr ints)))
(newline)
