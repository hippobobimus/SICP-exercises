(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; Vector and matrix arithmetic
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (m-row) (dot-product m-row v))
       m))

(define (transpose mat)
  (accumulate-n cons
                '()
                mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row)
           (matrix-*-vector cols m-row))
         m)))

;; Tests
(define v1 (list 1 2 3 4))
(define v2 (list 5 6 7 8))
(define m1 (list (list 10 20 30 40) (list 50 60 70 80) (list 90 100 110 120)))
(define m2 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define m3 (list (list 7 10) (list 8 11) (list 9 12)))

(newline)
(display "v1: ")
(display v1)
(newline)
(display "v2: ")
(display v2)
(newline)
(display "v1.v2 = ")
(display (dot-product v1 v2))
(newline)
(display "v1: ")
(display v1)
(newline)
(display "m1: ")
(display m1)
(newline)
(display "m1 * v1 = ")
(display (matrix-*-vector m1 v1))
(newline)
(display "m1: ")
(display m1)
(newline)
(display "Transposed m1: ")
(display (transpose m1))
(newline)
(display "m2: ")
(display m2)
(newline)
(display "m3: ")
(display m3)
(newline)
(display "m2 * m3 = ")
(display (matrix-*-matrix m2 m3))
(newline)
