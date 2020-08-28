;; Segment
(define (make-segment a b)
  (cons a b))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (length segment)
  (let ((a (start-segment segment))
        (b (end-segment segment)))
    (let ((a-x (x-point a))
          (a-y (y-point a))
          (b-x (x-point b))
          (b-y (y-point b)))
      (let ((dx (- b-x a-x))
            (dy (- b-y a-y)))
        (sqrt (+ (square dx) (square dy)))))))

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

;; Rectangle
(define (make-rect base side)
  (cons base side))

(define (base rectangle)
  (car rectangle))

(define (side rectangle)
  (cdr rectangle))

(define (width rectangle)
  (length (base rectangle)))

(define (height rectangle)
  (length (side rectangle)))

(define (perimeter-rect rectangle)
  (* 2 (+ (width rectangle)
          (height rectangle))))

(define (area-rect rectangle)
  (* (width rectangle)
     (height rectangle)))

;; Test
(let ((base (make-segment (make-point 1 1)
                          (make-point 8 1)))
      (side (make-segment (make-point 1 1)
                          (make-point 1 10))))
  (let ((r (make-rect base side)))
    (newline)
    (display "Rectangle:")
    (display r)
    (newline)
    (display "perimeter:")
    (display (perimeter-rect r))
    (newline)
    (display "area:")
    (display (area-rect r))))

