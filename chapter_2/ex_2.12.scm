;; Constructor
(define (make-interval a b)
  (cons a b))

;; Selectors
(define upper-bound cdr)

(define lower-bound car)

;; Percent between 0 and 1.0
;; Constructor
(define (make-center-percent c p)
  (make-interval (* c (- 1.0 p))
                 (* c (+ 1.0 p))))

;; Selectors
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i))
     2))

(define (percent i)
  (/ (/ (- (upper-bound i) (lower-bound i))
        2)
     (center i)))

;; Test
(define int-1 (make-center-percent 100 0.01))

(newline)
(display "Interval: ")
(display int-1)
(newline)
(display "Center: ")
(display (center int-1))
(newline)
(display "Percent: ")
(display (percent int-1))
(newline)
