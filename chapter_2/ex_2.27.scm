;; Takes a list and returns a list of the same elements in reverse order.
(define (reverse input-list)
  (define (iter temp-list l)
    (if (null? l)
      temp-list
      (iter (cons (car l) temp-list) (cdr l))))
  (iter '() input-list))

(define (deep-reverse items)
  (define (iter result x)
    (cond ((null? x)
           result)
          ((not (pair? x))
           x)
          (else (iter (cons (deep-reverse (car x))
                            result)
                      (cdr x)))))
  (iter '() items))

;; Test
(define list1 (list (list 1 2) (list 3 4)))

(newline)
(newline)
(display "List: ")
(display list1)
(newline)
(display "Reverse: ")
(display (reverse list1))
(newline)
(display "Deep reverse: ")
(display (deep-reverse list1))
(newline)

