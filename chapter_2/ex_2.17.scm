;; Returns the last-pair in a list
(define (last-pair l)
  (define (last-pair-iter m n)
    (if (null? n)
      m
      (last-pair-iter n (cdr n))))
  (if (null? l)
    (error "Cannot return last pair of an empty list.")
    (last-pair-iter l (cdr l))))

;; Test
(define list1 (list 2 5 7 9 23))
(define list2 (list 7))
(define list3 '())

(newline)
(display "List: ")
(display list1)
(newline)
(display "Last pair: ")
(display (last-pair list1))

(newline)
(display "List: ")
(display list2)
(newline)
(display "Last pair: ")
(display (last-pair list2))

(newline)
(display "List: ")
(display list3)
(newline)
(display "Last pair: ")
(display (last-pair list3))
