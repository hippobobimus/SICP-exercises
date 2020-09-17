(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; Reverse

(define (reverse-fr sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define (reverse-fl sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

;; Tests
(define s (list 1 2 3 4 5))

(newline)
(display "Sequence: ")
(display s)
(newline)
(display "Reverse using fold-right: ")
(display (reverse-fr s))
(newline)
(display "Reverse using fold-left: ")
(display (reverse-fl s))
