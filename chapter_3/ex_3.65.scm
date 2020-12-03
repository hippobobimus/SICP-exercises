#lang sicp
(#%require "streams.scm")

(define (square x) (* x x))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

(define (stream-limit stream tolerance)
  (define iterations 2)
  (define (helper s)
    (let ((s0 (stream-ref s 0))
          (s1 (stream-ref s 1)))
      (if (< (abs (- s0 s1)) tolerance)
          (begin
            (display "Stream elements before tolerance ")
            (display tolerance)
            (display " was reached: ")
            (display iterations)
            (newline)
            s1)
          (begin
            (set! iterations (+ iterations 1))
            (helper (stream-cdr s))))))
  (helper stream))

;; Test
(newline)
(display "--- INITIAL APPROXIMATION ---")
(newline)
(display-stream-head ln2-stream)
(newline)
(display (stream-limit ln2-stream 0.001))
(newline)

(newline)
(display "--- SINGLE EULER ACCELERATION ---")
(newline)
(display-stream-head (euler-transform ln2-stream))
(newline)
(display (stream-limit (euler-transform ln2-stream) 0.00001))
(newline)

(newline)
(display "--- TABLEAU SUPER-ACCELERATION ---")
(newline)
(display-stream-head (accelerated-sequence euler-transform ln2-stream))
(newline)
(display (stream-limit (accelerated-sequence euler-transform ln2-stream) 0.0000000001))
(newline)
