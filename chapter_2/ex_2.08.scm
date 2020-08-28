;; Constructor
(define (make-interval a b)
  (cons a b))

;; Selectors
(define upper-bound cdr)

(define lower-bound car)

;; Arithmetic
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (add-interval x
                (make-interval (- upper-bound y)
                               (- lower-bound y))))
