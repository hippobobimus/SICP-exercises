#lang sicp
(#%require racket/trace)

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (floyd-algorithm f x0)
  (define (floyd-iter tortoise hare)
    (cond ((null? hare) #f)
          ((eq? tortoise hare) #t)
          (else (floyd-iter (f tortoise) (f (f hare))))))
  (floyd-iter (f x0) (f (f x0))))

(define (cycle? l)
  (define (f x)
    (if (pair? x)
        (cdr x)
        '()))
  (floyd-algorithm f l))
        
;; TEST
(define l1 (list 'a 'b 'c 'd 'e))
(define l2 (make-cycle (list 'a 'b 'c 'd 'e)))
(define l3 (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr l3) (cdr l3))

(newline)
(display l1)
(newline)
(display "Cycle?: ")
(display (cycle? l1))
(newline)

(newline)
(display l2)
(newline)
(display "Cycle?: ")
(display (cycle? l2))
(newline)

(newline)
(display l3)
(newline)
(display "Cycle?: ")
(display (cycle? l3))
(newline)
(newline)

