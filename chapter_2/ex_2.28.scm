(define (fringe tree)
  (define (iter x result)
    (cond ((null? x)
           result)
          ((pair? x)
           (iter (car x)
                 (iter (cdr x)
                       result)))
          (else
            (cons x result))))
  (iter tree '()))

;; Test
(define x (list (list 1 2) (list 3 4)))

(newline)
(display "(fringe x): ")
(display (fringe x))
(newline)
(display "(fringe (list x x)): ")
(display (fringe (list x x)))
