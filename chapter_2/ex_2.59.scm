#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else
          (cons (car set1) (union-set (cdr set1) set2)))))

;; Test
(define set1 (list 1 2 3 4 5))
(define set2 (list 5 6 7 8 9))

(display "Set1: ")
(display set1)
(newline)
(display "Set2: ")
(display set2)
(newline)
(display "Union: ")
(display (union-set set1 set2))
(newline)
