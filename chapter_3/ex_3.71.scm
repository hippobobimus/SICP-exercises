#lang sicp
(#%require "streams.scm")
(#%require racket/trace)

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< (weight s1car) (weight s2car))
                   (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
                  ((> (weight s1car) (weight s2car))
                   (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))
                  (else
                    (cons-stream s1car
                                 (cons-stream s2car
                                              (merge-weighted (stream-cdr s1)
                                                              (stream-cdr s2)
                                                              weight)))))))))

(define (weighted-pairs s t weighting-func)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weighting-func)
      weighting-func)))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

;; Exercise
(define (r-weight pair)
  (+ (expt (car pair) 3)
     (expt (cadr pair) 3)))

(define (get-ramanujan-numbers stream)
  (let ((s0 (stream-ref stream 0))
        (s1 (stream-ref stream 1)))
    (let ((w0 (r-weight s0))
          (w1 (r-weight s1)))
      (if (= w0 w1)
          (cons-stream (list w0 s0 s1)
                       (get-ramanujan-numbers (stream-cdr (stream-cdr stream))))
          (get-ramanujan-numbers (stream-cdr stream))))))

(define ramanujan-numbers
  (get-ramanujan-numbers
    (weighted-pairs integers
                    integers
                    r-weight)))

(newline)
(display "Ramanujan numbers: ")
(newline)
(display-stream-head-n ramanujan-numbers 6)
(newline)
