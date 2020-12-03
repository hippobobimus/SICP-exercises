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
(define (triple-stream stream)
  (let ((s0 (stream-ref stream 0))
        (s1 (stream-ref stream 1))
        (s2 (stream-ref stream 2)))
    (cons-stream (list s0 s1 s2)
                 (triple-stream (stream-cdr stream)))))

(define (sum-of-squares pair)
  (+ (expt (car pair) 2) (expt (cadr pair) 2)))

(define sum-of-two-squares-in-3-ways
  (stream-map (lambda (x)
                (cons (sum-of-squares (car x)) x))
              (stream-filter (lambda (x)
                               (= (sum-of-squares (car x)) 
                                  (sum-of-squares (cadr x))
                                  (sum-of-squares (caddr x))))
                             (triple-stream
                               (weighted-pairs integers integers sum-of-squares)))))

(newline)
(display "Numbers that can be written as the sum of two squares in three different ways: ")
(newline)
(display-stream-head sum-of-two-squares-in-3-ways)
(newline)
