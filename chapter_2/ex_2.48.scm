#lang sicp

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

;; Constructor
(define (make-segment x0 y0 x1 y1)
  (cons (make-vect x0 y0)
        (make-vect x1 y1)))

(define start-segment car)

(define end-segment cdr)

;; Test
(define seg (make-segment 1 2 3 4))

(newline)
(display "Segment: ")
(display seg)
(newline)
(display "start: ")
(display (start-segment seg))
(newline)
(display "end: ")
(display (end-segment seg))
(newline)
