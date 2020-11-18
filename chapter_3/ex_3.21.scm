#lang sicp

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else 
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
          (set-front-ptr! queue (cdr (front-ptr queue)))
          queue)))

(define (print-queue queue)
  (display (front-ptr queue)))

;; Test
(define q1 (make-queue))

(display "Queue")
(newline)
(display "Standard method: ")
(display q1)
(newline)
(display "New method: ")
(print-queue q1)
(newline)
(newline)

(display "(insert-queue! q1 'a)")
(newline)
(display "Standard method: ")
(insert-queue! q1 'a)
(display "New method: ")
(print-queue q1)
(newline)
(newline)

(display "(insert-queue! q1 'b)")
(newline)
(display "Standard method: ")
(insert-queue! q1 'b)
(display "New method: ")
(print-queue q1)
(newline)
(newline)

(display "(delete-queue! q1)")
(newline)
(display "Standard method: ")
(delete-queue! q1)
(display "New method: ")
(print-queue q1)
(newline)
(newline)

(display "(delete-queue! q1)")
(newline)
(display "Standard method: ")
(delete-queue! q1)
(display "New method: ")
(print-queue q1)
(newline)
(newline)
