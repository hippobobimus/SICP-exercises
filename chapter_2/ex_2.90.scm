#lang racket
(require racket/trace)

(include "operation-table.scm")

;; Generic functions and constructors
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))

(define (negate x) (apply-generic 'negate x))
(define (=zero? x) (apply-generic '=zero? x))
(define (equ? x y) (apply-generic 'equ? x y))

;; *****
;; TERMS
;; *****
(define (install-term-package)
  ;; internal procedures
  ;; representation of terms
  (define (make-term order coeff) (list order coeff))
  (define (order-term term) (car term))
  (define (coeff-term term) (cadr term))
  (define (equ-term? t1 t2)
    (and (= (order-term t1) (order-term t2))
         (equ? (coeff-term t1) (coeff-term t2))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'term x))
  (put 'order '(term) order-term)
  (put 'coeff '(term) coeff-term)
  (put 'equ? '(term term) equ-term?)
  (put 'make 'term
       (lambda (order coeff) (tag (make-term order coeff))))
  'done)

(define (make-term order coeff)
  ((get 'make 'term) order coeff))

(define (order term)
  (apply-generic 'order term))

(define (coeff term)
  (apply-generic 'coeff term))

;; ****************
;; SPARSE TERMLISTS
;; ****************
(define (install-sparse-termlist-package)
  ;; internal procedures
  ;; representation of termlists
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-termlist terms) terms)
  
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
          (adjoin-term (make-term (mul (coeff t1) (coeff t2)))
                       (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (negate-terms L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t1 (first-term L)))
          (adjoin-term (make-term (order t1)
                                  (negate (coeff t1)))
                       (negate-terms (rest-terms L))))))

  (define (terms-zero? L)
    (cond ((empty-termlist? L) #t)
          ((= (coeff (first-term L)) 0)
           (terms-zero? (rest-terms L)))
          (else #f)))

  ;; interface to rest of system
  (define (tag x) (attach-tag 'sparse-termlist x))
  (put 'add '(sparse-termlist sparse-termlist)
       (lambda (x y) (tag (add-terms x y))))
  (put 'mul '(sparse-termlist sparse-termlist)
       (lambda (x y) (tag (mul-terms x y))))
  (put 'negate '(sparse-termlist)
       (lambda (x) (tag (negate-terms x))))
  (put '=zero? '(sparse-termlist) terms-zero?)
  (put 'make 'sparse-termlist
       (lambda (terms) (tag (make-termlist terms))))
  'done)

;; ***************
;; DENSE TERMLISTS
;; ***************
(define (install-dense-termlist-package)
  ;; internal procedures
  ;; representation of termlists
  (define (the-empty-termlist) '())
  (define (first-term term-list)
    (make-term (- (length term-list) 1)
               (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-termlist terms) terms)
  
  (define (adjoin-term term term-list)
        (cons (make-term coeff term) term-list))

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
          (adjoin-term (make-term (mul (coeff t1) (coeff t2)))
                       (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (negate-terms L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t1 (first-term L)))
          (adjoin-term (make-term (order t1)
                                  (negate (coeff t1)))
                       (negate-terms (rest-terms L))))))

  (define (terms-zero? L)
    (cond ((empty-termlist? L) #t)
          ((= (coeff (first-term L)) 0)
           (terms-zero? (rest-terms L)))
          (else #f)))

  ;; interface to rest of system
  (define (tag x) (attach-tag 'dense-termlist x))
  (put 'add '(dense-termlist dense-termlist)
       (lambda (x y) (tag (add-terms x y))))
  (put 'mul '(dense-termlist dense-termlist)
       (lambda (x y) (tag (mul-terms x y))))
  (put 'negate '(dense-termlist)
       (lambda (x) (tag (negate-terms x))))
  (put '=zero? '(dense-termlist) terms-zero?)
  (put 'make 'dense-termlist
       (lambda (terms) (tag (make-termlist terms))))
  'done)

;; ***********
;; POLYNOMIALS
;; ***********
(define (install-polynomial-package)
  (install-term-package)
  (install-dense-termlist-package)
  (install-sparse-termlist-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (make-from-dense-termlist variable term-list)
    (make-poly variable
               ((get 'make 'dense-termlist) term-list)))
  (define (make-from-sparse-termlist variable term-list)
    (make-poly variable
               ((get 'make 'sparse-termlist) term-list)))

  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (variable? v) (symbol? v))

  ;; poly operations
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add (term-list p1)
                        (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul (term-list p1)
                        (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  (define (=zero-poly? p)
    (=zero? (term-list p)))

  (define (negate-poly p)
    (make-poly (variable p)
               (negate (term-list p))))

  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2)))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'make-from-sparse 'polynomial
       (lambda (var terms) (tag (make-from-sparse-termlist var terms))))
  (put 'make-from-dense 'polynomial
       (lambda (var terms) (tag (make-from-dense-termlist var terms))))
  (put '=zero? '(polynomial) =zero-poly?)
  (put 'negate '(polynomial)
       (lambda (p) (tag (negate-poly p))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  'done)

(define (make-polynomial-from-sparse-termlist variable termlist)
  ((get 'make-from-sparse 'polynomial) variable termlist))

(define (make-polynomial-from-dense-termlist variable termlist)
  ((get 'make-from-dense 'polynomial) variable termlist))

;; ***************
;; REAL ARITHMETIC
;; ***************
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
  (put 'negate '(real)
       (lambda (x) (tag (- x))))
  'done)

;; TESTS
(display "Install polynomials package: ")
(install-polynomial-package)
(display "Install real number package: ")
(install-real-package)
(newline)

(trace get)

;; We assume polynomials are provided with terms arranged from highest to lowest order.
(define p0 (make-polynomial-from-dense-termlist 'x (list (make-term 2 0)
                                                         (make-term 1 0)
                                                         (make-term 0 0))))
(define p1 (make-polynomial-from-dense-termlist 'x (list (make-term 3 1)
                                                         (make-term 2 3)
                                                         (make-term 1 0)
                                                         (make-term 0 0))))
(define p2 (make-polynomial-from-dense-termlist 'x (list (make-term 4 3)
                                                         (make-term 3 0)
                                                         (make-term 2 1)
                                                         (make-term 1 2)
                                                         (make-term 0 0))))

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

