#lang racket
(require rnrs/mutable-pairs-6)
(require compatibility/mlist)
(require racket/trace)

;; Generic procedures
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (make-integer n)
  ((get 'make 'integer) n))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (make-real n)
  ((get 'make 'real) n))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (equ? x y) (apply-generic 'equ? x y))

(define (=zero? x) (apply-generic '=zero? x))

(define (raise x)
  (apply-generic 'raise x))

(define (drop x)
  (cond ((zero? (rank x)) x)
        ((equ? x (raise (project x)))
         (drop (project x)))
        (else x)))

(define (project x)
  (apply-generic 'project x))

(define (rank type)
  (apply-generic 'rank type))

;; Helper procedures
(define (attach-tag type-tag contents)
  (if (and (eq? type-tag 'real) (number? contents))
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'real)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (if (or (eq? op 'raise)
                  (eq? op 'project)
                  (eq? op 'equ?)
                  (eq? op '=zero?)
                  (eq? op 'rank))
              (apply proc (map contents args))
              (drop (apply proc (map contents args))))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (let ((rank1 (rank a1))
                      (rank2 (rank a2)))
                  (cond ((eq? rank1 rank2)
                         (error
                           "No method for these types -- APPLY-GENERIC"
                           (list op type-tags)))
                        ((< rank1 rank2)
                         (apply-generic op (raise a1) a2))
                        (else
                          (apply-generic op a1 (raise a2))))))
              (error
                "No method for these types -- APPLY-GENERIC"
                (list op type-tags)))))))

;; Operation table
(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                            (mcons (mcons key-2 value)
                                  (mcdr subtable)))))
            (set-mcdr! local-table
                      (mcons (mlist key-1
                                  (mcons key-2 value))
                            (mcdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; Type ranking
(define (install-type-rank-package)
  (put 'rank '(integer) (lambda (x) 0))
  (put 'rank '(rational) (lambda (x) 1))
  (put 'rank '(real) (lambda (x) 2))
  (put 'rank '(complex) (lambda (x) 3))
  'done)

;; Integer arithmetic
(define (install-integer-package)
  ;; internal procedures
  (define (make-int x)
    (quotient x 1))
  (define (int->rat x)
    (make-rational x 1))
  ;; interface to rest of the system
  (define (tag x)
    (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (quotient x y))))
  (put 'make 'integer
       (lambda (x) (tag (make-int x))))
  (put 'equ? '(integer integer) equal?)
  (put '=zero? '(integer) zero?)
  (put 'raise '(integer) int->rat)
  'done)

;; Rational arithmetic
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
    (and (equal? (* (numer x) (denom y))
                 (* (numer y) (denom x)))))
  (define (=zero? x)
    (and (zero? (numer x))
         (not (zero? (denom x)))))
  (define (rat->real x)
    (/ (numer x) (denom x)))
  (define (project->int x)
    (make-integer (round (/ (numer x) (denom x)))))
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
  (put 'project '(rational) project->int)
  'done)

;; Real arithmetic
(define (install-real-package)
  ;; internal procedures
  (define (real->cplx x)
    (make-complex-from-real-imag x 0))
  (define (project->int x)
    (make-integer (round x)))
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
  (put 'raise '(real) real->cplx)
  (put 'project '(real) project->int)
  'done)

;; Rectangular package
(define (square x) (* x x))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y)
    (cond ((< (rank x) 2)
           (make-from-real-imag (raise x) y))
          ((< (rank y) 2)
           (make-from-real-imag x (raise y)))
          (else (cons x y))))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  (define (=zero? z)
    (and (zero? (real-part z))
         (zero? (imag-part z))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put '=zero? '(rectangular) =zero?)
  'done)

;; Polar package
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (=zero? z)
    (zero? (magnitude z)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put '=zero? '(polar) =zero?)
  'done)

;; Complex arithmetic
(define (install-complex-package)
  (install-rectangular-package)
  (install-polar-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ? z1 z2)
    (and (equal? (real-part z1) (real-part z2))
         (equal? (imag-part z1) (imag-part z2))))
  (define (project->real z)
    (real-part z))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex) =zero?)
  (put 'project '(complex) project->real)
  'done)

;; Test
(display "Install type-rank-package: ")
(install-type-rank-package)
(display "Install integer-package: ")
(install-integer-package)
(display "Install rational-package: ")
(install-rational-package)
(display "Install real-package: ")
(install-real-package)
(display "Install complex-package: ")
(install-complex-package)

(define i0 (make-integer 0))
(define i1 (make-integer 9))
(define r0 (make-rational 0 1))
(define r1 (make-rational 1 2))
(define r2 (make-rational 3 4))
(define z0rect (make-complex-from-real-imag 0 0))
(define z0polar (make-complex-from-mag-ang 0 0))
(define z1 (make-complex-from-real-imag 3 4))
(define z2 (make-complex-from-real-imag 7 9))
(define z3 (make-complex-from-real-imag 9 0))

(display "Complex number: ")
(display z1)
(newline)
(display "Real part: ")
(display (real-part z1))
(newline)
(display "Imaginary part: ")
(display (imag-part z1))
(newline)
(display "Magnitude: ")
(display (magnitude z1))
(newline)
(display "Angle: ")
(display (angle z1))
(newline)
(display "9 + 23: ")
(display (add 9 23))
(newline)
(display "int 7 + int 9: ")
(display (add (make-integer 7) (make-integer 9)))
(newline)
(display "1 = 1: ")
(display (equ? 1 1))
(newline)
(display "1 = 2: ")
(display (equ? 1 2))
(newline)
(display "1/2 = 1/2: ")
(display (equ? r1 r1))
(newline)
(display "1/2 = 3/4: ")
(display (equ? r1 r2))
(newline)
(display "z1 = z1: ")
(display (equ? z1 z1))
(newline)
(display "z1 = z2: ")
(display (equ? z1 z2))
(newline)
(display "(=zero? 0): ")
(display (=zero? 0))
(newline)
(display "(=zero? 3): ")
(display (=zero? 3))
(newline)
(display "(=zero? r0): ")
(display (=zero? r0))
(newline)
(display "(=zero? r1): ")
(display (=zero? r1))
(newline)
(display "(=zero? z0rect): ")
(display (=zero? z0rect))
(newline)
(display "(=zero? z0polar): ")
(display (=zero? z0polar))
(newline)
(display "(=zero? z1): ")
(display (=zero? z1))
(newline)
(display "Start with integer: ")
(display i1)
(newline)
(display "Raise -> ")
(display (raise i1))
(newline)
(display "Raise -> ")
(display (raise (raise i1)))
(newline)
(display "Raise -> ")
(display (raise (raise (raise i1))))
(newline)
(display "Rank of integer: ")
(display (rank i1))
(newline)
(display "Rank of rational: ")
(display (rank r1))
(newline)
(display "Rank of real: ")
(display (rank 7))
(newline)
(display "Rank of complex: ")
(display (rank z1))
(newline)
(display "3 + z1: ")
(display (add 3 z1))
(newline)
(display "r1 + z1: ")
(display (add r1 z1))
(newline)
(display "r1 + i1: ")
(display (add r1 i1))
(newline)
(display "Project r1: ")
(newline)
(display r1)
(newline)
(display (project r1))
(newline)
(display "Project 7: ")
(newline)
(display 7)
(newline)
(display (project 7))
(newline)
(display "Project z1: ")
(newline)
(display z1)
(newline)
(display (project z1))
(newline)
(display "Drop z1: ")
(newline)
(display z1)
(newline)
(display (drop z1))
(newline)
(display "Drop z3: ")
(newline)
(display z3)
(newline)
(display (drop z3))
(newline)
(display "Drop 1.5 + 0i: ")
(newline)
(display (drop (make-complex-from-real-imag 1.5 0)))
(newline)
(display "9 + (3 + 0i): ")
(display (add 9 (make-complex-from-real-imag 3 0)))
(newline)
