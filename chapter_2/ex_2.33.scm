(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; Map
(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              '()
              sequence))

;; Append
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
              
;; Length
(define (inc x) (+ x 1))

(define (length sequence)
  (accumulate (lambda (x y)
                (+ 1 y))
              0
              sequence))

;; Tests
(define l1 (list 1 2 3 4))
(define l2 (list 5 6 7 8))

(newline)
(display "List: ")
(display l1)
(newline)
(display "Maps to square: ")
(display (map square l1))
(newline)
(display "List 1: ")
(display l1)
(newline)
(display "List 2: ")
(display l2)
(newline)
(display "Append list 1 to list 2: ")
(display (append l1 l2))
(newline)
(display "List: ")
(display l1)
(newline)
(display "Length: ")
(display (length l1))
