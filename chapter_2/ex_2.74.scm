#lang racket

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

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

;; East division
(define (install-east-div-pkg)
  ;; internal procedures
  (define (get-name record) (car record))
  (define (get-salary record) (cadr record))
  (define (get-record name file)
    (cond ((null? file)
           '())
          ((eq? name (get-name (car file)))
           (car file))
          (else (get-record name (cdr file)))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'east x))
  (put 'get-salary 'east get-salary)
  (put 'get-record 'east 
       (lambda (x y)
         (let ((record (get-record x y)))
           (if (null? record)
               record
               (tag record)))))
  'done)

;; West division
(define (install-west-div-pkg)
  ;; internal procedures
  (define (get-name record) (cadr record))
  (define (get-salary record) (car record))
  (define (get-record name file)
    (cond ((null? file)
           '())
          ((eq? name (get-name (car file)))
           (car file))
          (else (get-record name (cdr file)))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'west x))
  (put 'get-salary 'west get-salary)
  (put 'get-record 'west 
       (lambda (x y)
         (let ((record (get-record x y)))
           (if (null? record)
               record
               (tag record)))))
  'done)

;; Generics
(define (get-record name file)
  ((get 'get-record (type-tag file)) name (contents file)))

(define (get-salary name file)
  (let ((record (get-record name file)))
    ((get 'get-salary (type-tag record)) (contents record))))

(define (find-employee-record name files)
  (if (null? files)
      (display "Employee not found.")
      (let ((record (get-record name (car files))))
        (if (null? record)
            (find-employee-record name (cdr files))
            record))))

;; Test
(install-east-div-pkg)
(install-west-div-pkg)
(define east-personnel '(east (John 20000) (Anne 25000) (Fred 15000)))
(define west-personnel '(west (20000 Lou) (19000 Clive) (34000 Edith)))
(define personnel-files (list east-personnel west-personnel))

(display (get-record 'John east-personnel))
(newline)
(display (get-record 'Clive west-personnel))
(newline)
(display (get-salary 'Anne east-personnel))
(newline)
(display "Find Clive: ")
(display (find-employee-record 'Clive personnel-files))
(newline)
