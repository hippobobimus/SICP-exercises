#lang sicp

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (cycle? l)
  (let ((history '()))
    (define (cycle-1 lst)
      (cond ((null? lst) #f)
            ((memq lst history) #t)
            (else 
              (begin (set! history (cons lst history))
                     (cycle-1 (cdr lst))))))
    (cycle-1 l)))

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
