#lang sicp

;; Constructor and selectors
(define make-vect cons)

(define xcor-vect car)

(define ycor-vect cdr)

;; Version 1
(define (make-frame-1 origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame-1 car)

(define edge1-frame-1 cadr)

(define edge2-frame-1 caddr)

;; Version 2
(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define origin-frame-2 car)

(define edge1-frame-2 cadr)

(define edge2-frame-2 cddr)

;; Test
(define origin (make-vect 1 2))
(define edge1 (make-vect 3 4))
(define edge2 (make-vect 5 2))
(define frame1 (make-frame-1 origin edge1 edge2))
(define frame2 (make-frame-2 origin edge1 edge2))

(display "Origin: ")
(display origin)
(newline)
(display "Edge1: ")
(display edge1)
(newline)
(display "Edge2: ")
(display edge2)
(newline)
(display "Check version 1")
(newline)
(display "Origin: ")
(display (origin-frame-1 frame1))
(newline)
(display "Edge1: ")
(display (edge1-frame-1 frame1))
(newline)
(display "Edge2: ")
(display (edge2-frame-1 frame1))
(newline)
(display "Check version 2")
(newline)
(display "Origin: ")
(display (origin-frame-2 frame2))
(newline)
(display "Edge1: ")
(display (edge1-frame-2 frame2))
(newline)
(display "Edge2: ")
(display (edge2-frame-2 frame2))
(newline)

