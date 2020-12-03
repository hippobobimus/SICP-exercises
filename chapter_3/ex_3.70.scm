#lang sicp
(#%require "streams.scm")

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

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

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

;; Part (a)
(define stream-a (weighted-pairs integers
                                 integers
                                 (lambda (pair)
                                   (+ (car pair)
                                      (cadr pair)))))

(display "Part (a): ")
(newline)
(display-stream-head stream-a)
(newline)

;; Part (b)
(define temp-b (weighted-pairs integers
                               integers
                               (lambda (pair)
                                 (+ (* 2 (car pair))
                                    (* 3 (cadr pair))
                                    (* 5 (car pair) (cadr pair))))))

(define stream-b (stream-filter (lambda (pair)
                                  (let ((i (car pair))
                                        (j (cadr pair)))
                                    (not (or (= (remainder i 2) 0)
                                             (= (remainder j 2) 0)
                                             (= (remainder i 3) 0)
                                             (= (remainder j 3) 0)
                                             (= (remainder i 5) 0)
                                             (= (remainder j 5) 0)))))
                                temp-b))

(display "Part (b): ")
(newline)
(display-stream-head stream-b)
(newline)
