#lang racket
(require racket/trace)

(include "operation-table.scm")

;; Generic functions and constructors
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (negate x) (apply-generic 'negate x))
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
  (define (negate-terms L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t1 (first-term L)))
          (adjoin-term (make-term (order t1)
                                  (negate (coeff t1)))
                       (negate-terms (rest-terms L))))))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                        (div-terms (add-terms L1
                                              (negate-terms (mul-term-by-all-terms (make-term new-o new-c)
                                                                                   L2)))
                                   L2)))
                  (list (add-terms (list (make-term new-o new-c))
                                   (car rest-of-result))
                        (cadr rest-of-result))))))))

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

  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2)))

  (define (=zero-poly? p)
    (terms-zero? (term-list p)))
  (define (terms-zero? L)
    (cond ((empty-termlist? L) #t)
          ((= (coeff (first-term L)) 0)
           (terms-zero? (rest-terms L)))
          (else #f)))

  (define (negate-poly p)
    (make-poly (variable p)
               (negate-terms (term-list p))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (map (lambda (x) (make-poly (variable p1) x))
             (div-terms (term-list p1)
                        (term-list p2)))
        (error "Polys not in same var -- DIV-POLY"
               (list p1 p2))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) =zero-poly?)
  (put 'negate '(polynomial)
       (lambda (p) (tag (negate-poly p))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (map tag (div-poly p1 p2))))
  'done)

;; Rational arithmetic
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d) (cons n d))
;;  (define (make-rat n d)
;;    (let ((g (gcd n d)))
;;      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  (define (equ? x y)
    (and (equ? (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))))
  (define (=zero? x)
    (and (=zero? (numer x))
         (not (=zero? (denom x)))))
  (define (rat->real x)
    (/ (numer x) (denom x)))
;;  (define (project->int x)
;;    (make-integer (round (/ (numer x) (denom x)))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) =zero?)
  (put 'raise '(rational) rat->real)
;;  (put 'project '(rational) project->int)
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;; Real arithmetic
(define (install-real-package)
  ;; internal procedures

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
  (put 'negate '(real)
       (lambda (x) (tag (- x))))
  'done)

;; TESTS
(display "Install polynomials package: ")
(install-polynomial-package)
(display "Install real number package: ")
(install-real-package)
(display "Install rational number package: ")
(install-rational-package)
(newline)


;; We assume polynomials are provided with terms arranged from highest to lowest order.
(define p0 (make-polynomial 'x '((2 0) (3 0))))
(define p1 (make-polynomial 'x '((2 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 1))))
(define p3 (make-polynomial 'x '((5 1) (0 -1))))
(define p4 (make-polynomial 'x '((2 1) (0 -1))))
(define rf (make-rational p2 p1))

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
(display "subtract: ")
(display (sub p1 p2))
(newline)
(newline)
(display "Poly 3: ")
(display p3)
(newline)
(display "Poly 4: ")
(display p4)
(newline)
(display "divide: ")
(display (div p3 p4))
(newline)
(newline)

(display "Rational poly: ")
(display rf)
(newline)
(display "add to itself: ")
(display (add rf rf))
(newline)
