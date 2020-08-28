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

;; Arithmetic
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (add-interval x
                (make-interval (- upper-bound y)
                               (- lower-bound y))))

(define (mul-interval x y)
  (let ((lbx (lower-bound x))
        (ubx (upper-bound x))
        (lby (lower-bound y))
        (uby (upper-bound y)))
    (cond ((and (>= lbx 0) (>= lby 0))                      ;; + +
           (make-interval (* lbx lby)
                          (* ubx uby)))
          ((and (<= ubx 0) (<= uby 0))                      ;; - -
           (make-interval (* ubx uby)
                          (* lbx lby)))
          ((and (< lbx 0) (> ubx 0) (< lby 0) (> uby 0))    ;; 0 0
           (make-interval (min (* lbx uby) (* lby ubx))
                          (max (* ubx uby) (* lbx lby))))
          ((and (>= lbx 0) (<= uby 0))                      ;; + -
           (make-interval (* ubx lby)
                          (* lbx uby)))
          ((and (<= ubx 0) (>= lby 0))                      ;; - +
           (make-interval (* lbx uby)
                          (* ubx lby)))
          ((and (< lbx 0) (> ubx 0) (>= lby 0))             ;; 0 +
           (make-interval (* lbx uby)
                          (* ubx uby)))
          ((and (>= lbx 0) (< lby 0) (> uby 0))             ;; + 0
           (make-interval (* ubx lby)
                          (* ubx uby)))
          ((and (< lbx 0) (> ubx 0) (<= uby 0))             ;; 0 -
           (make-interval (* ubx lby)
                          (* lbx lby)))
          ((and (<= ubx 0) (< lby 0) (> uby 0))             ;; - 0
           (make-interval (* lbx uby)
                          (* lbx lby))))))

(define (div-interval x y)
  (if (<= (* (upper-bound y) (lower-bound y)) 0)
      (display "error: divisor interval spans zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;; Test
(define int-a (make-center-percent 200 0.03))
(define int-b (make-center-percent 200 0.03))
(define int-c (div-interval int-a int-a))
(define int-d (div-interval int-a int-b))

(newline)
(display "Interval: ")
(display int-a)
(newline)
(display "Center: ")
(display (center int-a))
(newline)
(display "Percent: ")
(display (percent int-a))
(newline)
(display "Interval: ")
(display int-b)
(newline)
(display "Center: ")
(display (center int-b))
(newline)
(display "Percent: ")
(display (percent int-b))
(newline)
(display "A/A: ")
(display int-c)
(newline)
(display "Interval: ")
(display int-c)
(newline)
(display "Center: ")
(display (center int-c))
(newline)
(display "Percent: ")
(display (percent int-c))
(newline)
(display "A/B: ")
(display int-d)
(newline)
(display "Interval: ")
(display int-d)
(newline)
(display "Center: ")
(display (center int-d))
(newline)
(display "Percent: ")
(display (percent int-d))
(newline)
