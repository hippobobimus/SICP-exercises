#lang racket

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1))
                (x2 (car set2)))
            (cond ((< x1 x2)
                   (cons x1
                         (union-set (cdr set1) set2)))
                  ((= x1 x2)
                   (cons x1
                         (union-set (cdr set1) (cdr set2))))
                  (else (cons x2
                              (union-set set1 (cdr set2)))))))))

;; Test
(define set1 (list 1 3 5 7 9))
(define set2 (list 2 3 4 6 8))

(display "Set1: ")
(display set1)
(newline)
(display "Set2: ")
(display set2)
(newline)
(display "Union: ")
(display (union-set set1 set2))
(newline)
