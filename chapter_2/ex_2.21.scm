(define (square-list-old items)
  (if (null? items)
    '()
    (cons (square (car items))
          (square-list-old (cdr items)))))

(define (square-list items)
  (map square items))

;; Test
(newline)
(display "(square-list-old '(1 3 8 9)): ")
(display (square-list-old '(1 3 8 9)))
(newline)
(display "(square-list '(1 3 8 9)): ")
(display (square-list '(1 3 8 9)))
