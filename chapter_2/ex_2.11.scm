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

(define (old-mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (<= (* (upper-bound y) (lower-bound y)) 0)
      (display "error: divisor interval spans zero")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;; Testing

(define (interval-equal? x y)
  (and (equal? (lower-bound x) (lower-bound y))
       (equal? (upper-bound x) (upper-bound y))))

(define posi-interval-1 (make-interval 0 9))
(define nega-interval-1 (make-interval -7 0))
(define span-interval-1 (make-interval -5 3))

(define posi-interval-2 (make-interval 1 8))
(define nega-interval-2 (make-interval -102 -18))
(define span-interval-2 (make-interval -93 77))

(define (check-mul x y)
  (if (interval-equal? (mul-interval x y)
                       (old-mul-interval x y))
    (begin
       (newline)
       (display "Ok: ")
       (display x)
       (display " ")
       (display y))
    (begin
       (newline)
       (display "Error: ")
       (display x)
       (display " ")
       (display y))))

(check-mul posi-interval-1 posi-interval-2)
(check-mul posi-interval-1 nega-interval-2)
(check-mul posi-interval-1 span-interval-2)
(check-mul nega-interval-1 posi-interval-2)
(check-mul nega-interval-1 nega-interval-2)
(check-mul nega-interval-1 span-interval-2)
(check-mul span-interval-1 posi-interval-2)
(check-mul span-interval-1 nega-interval-2)
(check-mul span-interval-1 span-interval-2)

(check-mul posi-interval-1 posi-interval-1)
(check-mul posi-interval-1 nega-interval-1)
(check-mul posi-interval-1 span-interval-1)
(check-mul nega-interval-1 posi-interval-1)
(check-mul nega-interval-1 nega-interval-1)
(check-mul nega-interval-1 span-interval-1)
(check-mul span-interval-1 posi-interval-1)
(check-mul span-interval-1 nega-interval-1)
(check-mul span-interval-1 span-interval-1)

(check-mul posi-interval-2 posi-interval-1)
(check-mul posi-interval-2 nega-interval-1)
(check-mul posi-interval-2 span-interval-1)
(check-mul nega-interval-2 posi-interval-1)
(check-mul nega-interval-2 nega-interval-1)
(check-mul nega-interval-2 span-interval-1)
(check-mul span-interval-2 posi-interval-1)
(check-mul span-interval-2 nega-interval-1)
(check-mul span-interval-2 span-interval-1)

(check-mul posi-interval-2 posi-interval-2)
(check-mul posi-interval-2 nega-interval-2)
(check-mul posi-interval-2 span-interval-2)
(check-mul nega-interval-2 posi-interval-2)
(check-mul nega-interval-2 nega-interval-2)
(check-mul nega-interval-2 span-interval-2)
(check-mul span-interval-2 posi-interval-2)
(check-mul span-interval-2 nega-interval-2)
(check-mul span-interval-2 span-interval-2)
