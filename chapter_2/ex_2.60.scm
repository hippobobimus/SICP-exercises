#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(define (intersection-set-original set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set-original (cdr set1) set2)))
        (else (intersection-set-original (cdr set1) set2))))

(define (intersection-set set1 set2)
  (let ((union (union-set set1 set2))
        (checklist (intersection-set-original set1 set2)))
    (intersection-set-original union checklist)))

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
(display "Intersection: ")
(display (intersection-set set1 set2))
(newline)
(display "Adjoin 5 to set1: ")
(display (adjoin-set 5 set1))
(newline)
