(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (append (list (car s)) x))
                          rest)))))

;; Tests
(define l (list 1 2 3))
(newline)
(display (subsets l))
