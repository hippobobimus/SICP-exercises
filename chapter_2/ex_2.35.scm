(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-tree t)
  (cond ((null? t) '())
        ((not (pair? t)) (list t))
        (else (append (enumerate-tree (car t))
                      (enumerate-tree (cdr t))))))

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x) 1) (enumerate-tree t))))

;; Test
(define l (list 1 (list 1 2 3) (list 5) '() (list (list 1) (list 3 4))))

(newline)
(display "List: ")
(newline)
(display l)
(newline)
(display "Count-leaves: ")
(display (count-leaves l))
