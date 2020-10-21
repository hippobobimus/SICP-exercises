#lang racket
(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

;; Generic procedures
(define (add-old x y) (apply-generic 'add x y))
(define (add . args) (apply-generic 'add args))

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;; Helper procedures
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (coerce type-tags args)
  (define (iter tags)
    (display "Tags: ")
    (display tags)
    (newline)
    (if (null? tags)
        #f
        (let ((target-type (car tags)))
          (display "target-type: ")
          (display target-type)
          (newline)
          (let ((coercions (map (lambda (source-type)
                                  (if (eq? source-type target-type)
                                      (lambda (x) x)
                                      (get-coercion source-type target-type)))
                                type-tags)))
            (display "coercions: ")
            (display coercions)
            (newline)
            (if (memq #f coercions)
                (iter (cdr tags))
                (map (lambda (coercion arg) (coercion arg))
                     coercions
                     args))))))
  (iter type-tags))

(define (elements-equal? l)
  (define (elements-equal-iter reference check-list)
  (cond ((null? check-list) #t)
        ((eq? (car check-list) reference)
         (elements-equal-iter reference (cdr check-list)))
        (else #f)))
  (if (null? l)
      #t
      (elements-equal-iter (car l) (cdr l))))

(define (apply-generic op . args)
  (display "Args: ")
  (display args)
  (newline)
  (let ((type-tags (map type-tag args)))
    (display "Type-tags: ")
    (display type-tags)
    (newline)
    (let ((proc (get op type-tags)))
      (display "Proc: ")
      (display proc)
      (newline)
      (cond (proc
             (begin (display "proc args: ")
                    (display (map contents args))
                    (newline)
                    (apply proc (map contents args))))
            ((elements-equal? type-tags)
             (error "No method for these types -- APPLY-GENERIC"
                    (list op type-tags)))
            (else
              (let ((coerced-args (coerce type-tags args)))
                (display "coerced args: ")
                (display coerced-args)
                (newline)
                (if coerced-args
                    (apply apply-generic (cons op coerced-args))
                    (error "No method for these types -- APPLY-GENERIC"
                           (list op type-tags)))))))))

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

;; Coercion table
(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

;; Ordinary arithmetic
(define (install-scheme-number-package)
  ;; internal procedures
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
  ;; interface to the rest of the system
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (args) (tag (apply + args))))
  (put-coercion 'scheme-number 'complex scheme-number->complex)
  'done)

;; Rectangular package
(define (square x) (* x x))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
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
  (define (add-complex args)
    (make-from-real-imag (accumulate + 0 (map real-part args))
                         (accumulate + 0 (map imag-part args))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

;; Test
(display "Install scheme-number-package: ")
(install-scheme-number-package)
(display "Install complex-package: ")
(install-complex-package)

(define z0rect (make-complex-from-real-imag 0 0))
(define z0polar (make-complex-from-mag-ang 0 0))
(define z1 (make-complex-from-real-imag 3 4))
(define z2 (make-complex-from-real-imag 7 9))

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
(display (add-old 9 23))
(newline)
(display "3 + z1: ")
(display (add 3 z1))
(newline)
(display "3 + z1 + z2 + 5: ")
(display (add 3 z1 z2 5))
(newline)

