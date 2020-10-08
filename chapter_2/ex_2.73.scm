#lang racket

;; Table
(define proc-table '())

(define (put op type item)
  (set! proc-table (cons (list op type item)
                         proc-table)))

(define (get op type)
  (define (get-1 table)
    (if (null? table)
        (error "No method for this type -- GET" (list op type))
        (let ((element (car table)))
          (if (and (eq? op (car element))
                   (eq? type (cadr element)))
              (caddr element)
              (get-1 (cdr table))))))
  (get-1 proc-table))

;; Generic deriv procedure
(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr) (if (same-variable? expr var) 1 0))
        (else ((get 'deriv (operator expr)) (operands expr)
                                            var))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (operator expr) (car expr))

(define (operands expr) (cdr expr))

;; Sum package
(define (install-sum-package)
  ;; internal procedures
  (define (=number? expr num)
    (and (number? expr) (= expr num)))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (deriv-sum expr var)
    (make-sum (deriv (addend expr)
                     var)
              (deriv (augend expr)
                     var)))
  ;; interface to the rest of the system
  (put 'deriv '+ deriv-sum)
  'done)

;; Product package
(define (install-product-package)
  ;; internal procedures
  (define (=number? expr num)
    (and (number? expr) (= expr num)))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (deriv-prod expr var)
    (make-sum
      (make-product (multiplier expr)
                    (deriv (multiplicand expr) var))
      (make-product (deriv (multiplier expr) var)
                    (multiplicand expr))))
  ;; interface to the rest of the system
  (put 'deriv '* deriv-prod)
  'done)

;; Exponentiation package
(define (install-exp-package)
  ;;internal procedures
  (define (=number? expr num)
    (and (number? expr) (= expr num)))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (make-exponentiation b e)
    (cond ((=number? b 0) 0)
          ((=number? e 0) 1)
          ((and (number? b) (number? e)) (exp b e))
          (else (list '** b e))))
  (define (base e) (car e))
  (define (exponent e) (cadr e))
  (define (deriv-exp expr var)
    (make-product (make-product (exponent expr)
                                (make-exponentiation (base expr)
                                                     (make-sum (exponent expr)
                                                               '-1)))
                  (deriv (base expr) var)))
  ;; interface to the rest of the system
  (put 'deriv '** deriv-exp)
  'done)

;; Test
;;(define expr '(* x y (+ x 3)))
;;(define expr '(* x (* y (+ x 3))))
(define expr '(** x (* y (+ x 3))))

(install-sum-package)
(install-product-package)
(install-exp-package)
(display "Expression: ")
(display expr)
(newline)
(display "Derivative w.r.t. x: ")
(display (deriv expr 'x))
(newline)
