#lang racket
(require racket/trace)

(include "operation-table.scm")

;; Generic functions and constructors
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))

(define (=zero? x) (apply-generic '=zero? x))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;; Polynomials
(define (install-polynomial-package)
  ;; internal procedures

  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (variable? v) (symbol? v))

  ;; representation of terms and term lists
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1)) (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term t1
                                  (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term t2
                                  (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term (make-term (order t1)
                                              (add (coeff t1) (coeff t2)))
                                   (add-terms (rest-terms L1)
                                              (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term (make-term (+ (order t1) (order t2))
                                  (mul (coeff t1) (coeff t2)))
                       (mul-term-by-all-terms t1 (rest-terms L))))))

  ;; poly operations
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  (define (=zero-poly? p)
    (terms-zero? (term-list p)))
  (define (terms-zero? L)
    (cond ((empty-termlist? L) #t)
          ((= (coeff (first-term L)) 0)
           (terms-zero? (rest-terms L)))
          (else #f)))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) =zero-poly?)
  'done)

;; Real arithmetic
(define (install-real-package)
  ;; interface to rest of the system
  (define (tag x)
    (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (tag x)))
  (put 'equ? '(real real) equal?)
  (put '=zero? '(real) zero?)
  'done)

;; TESTS
(display "Install polynomials package: ")
(install-polynomial-package)
(display "Install real number package: ")
(install-real-package)
(newline)

;; We assume polynomials are provided with terms arranged from highest to lowest order.
(define p0 (make-polynomial 'x '((2 0) (3 0))))
(define p1 (make-polynomial 'x '((3 1) (2 3))))
(define p2 (make-polynomial 'x '((4 3) (2 1) (1 2))))

(display "Poly: ")
(display p0)
(newline)
(display "zero?: ")
(display (=zero? p0))
(newline)

(newline)
(display "Poly 1: ")
(display p1)
(newline)
(display "Poly 2: ")
(display p2)
(newline)
(display "add: ")
(display (add p1 p2))
(newline)

