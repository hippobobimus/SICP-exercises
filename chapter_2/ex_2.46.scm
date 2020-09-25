#lang sicp
(#%require sicp-pict)

;; Constructor and selectors
(define make-vect cons)

(define xcor-vect car)

(define ycor-vect cdr)

;; Procedures
(define (add-vect v w)
  (make-vect (+ (xcor-vect v)
                (xcor-vect w))
             (+ (ycor-vect v)
                (ycor-vect w))))

(define (sub-vect v w)
  (make-vect (- (xcor-vect v)
                (xcor-vect w))
             (- (ycor-vect v)
                (ycor-vect w))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;; Test
(define v (make-vect 2 3))
(define w (make-vect 1 4))

(newline)
(display "v: ")
(display v)
(newline)
(display "w: ")
(display w)
(newline)
(display "v + w = ")
(display (add-vect v w))
(newline)
(display "v - w = ")
(display (sub-vect v w))
(newline)
(display "3 * v = ")
(display (scale-vect 3 v))
(newline)
