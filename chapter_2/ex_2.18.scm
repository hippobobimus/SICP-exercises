;; Takes a list and returns a list of the same elements in reverse order.
(define (reverse input-list)
  (define (iter temp-list l)
    (if (null? l)
      temp-list
      (iter (cons (car l) temp-list) (cdr l))))
  (iter '() input-list))

;; Test
(define list1 (list 1 3 8 9))
(define list2 (list 7))
(define list3 '())
(define list4 (list 2 4 9 3 0 2 1))

(newline)
(display "List: ")
(display list1)
(newline)
(display "Reverse: ")
(display (reverse list1))

(newline)
(display "List: ")
(display list2)
(newline)
(display "Reverse: ")
(display (reverse list2))

(newline)
(display "List: ")
(display list3)
(newline)
(display "Reverse: ")
(display (reverse list3))

(newline)
(display "List: ")
(display list4)
(newline)
(display "Reverse: ")
(display (reverse list4))

