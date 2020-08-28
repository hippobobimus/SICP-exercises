;; Segment
(define (make-segment a b)
  (cons a b))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

;; Point
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; Midpoint
(define (midpoint-segment s)
  (let ((a (start-segment s))
        (b (end-segment s)))
    (let ((a-x (x-point a))
          (a-y (y-point a))
          (b-x (x-point b))
          (b-y (y-point b)))
      (make-point (/ (+ b-x a-x) 2) (/ (+ b-y a-y) 2)))))

;; Test
(let ((a (make-point 1 2))
      (b (make-point 17 9)))
  (let ((s (make-segment a b)))
    (newline)
    (display "Point a: ")
    (print-point a)
    (newline)
    (display "Point b: ")
    (print-point b)
    (newline)
    (display "Midpoint: ")
    (print-point (midpoint-segment s))))
