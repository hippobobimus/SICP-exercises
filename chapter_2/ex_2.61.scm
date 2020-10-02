#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((or (null? set) (< x (car set)))
         (cons x set))
        ((= x (car set))
         set)
        (else (cons (car set) (adjoin-set x (cdr set))))))

;; Test
(define set1 (list 1 2 4 5))

(display "Set: ")
(display set1)
(newline)
(display "Adjoin 2: ")
(display (adjoin-set 2 set1))
(newline)
(display "Adjoin 3: ")
(display (adjoin-set 3 set1))
(newline)
(display "Adjoin 9: ")
(display (adjoin-set 9 set1))
(newline)
