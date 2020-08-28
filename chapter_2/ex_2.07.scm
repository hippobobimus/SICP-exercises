;; Constructor
(define (make-interval a b)
  (cons a b))

;; Selectors
(define (upper-bound x)
  (cdr x))

(define (upper-bound x)
  (car x))

