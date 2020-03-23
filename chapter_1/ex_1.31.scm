(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product term a next b)  ; Iterative process.
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))


(define (identity x) x)

(define (inc x) (+ x 1))

(define (factorial n)
  (product identity 1 inc n))


(define (pi-product a b)
  (define (pi-term k)
    (/ (* (+ k 2) (+ k 4)) (square (+ k 3))))
  (define (pi-next x)
    (+ x 2))
  (product pi-term a pi-next b))

(define (pi-calc n)
  (* 4 (pi-product 0.0 n)))
