#lang racket

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
            (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

(define (get-real-part x) (apply-generic 'real-part x))
(define (get-imag-part x) (apply-generic 'imag-part x))
(define (get-magnitude x) (apply-generic 'magnitude x))
(define (get-angle x) (apply-generic 'angle x))

;; Test
(define n (make-from-mag-ang 2 30))

(display (get-real-part n))
(newline)
(display (get-imag-part n))
(newline)
(display (get-magnitude n))
(newline)
(display (get-angle n))
(newline)
