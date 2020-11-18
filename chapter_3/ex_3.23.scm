#lang sicp
(#%require racket/trace)

;; Links
(define (make-link value prev next) (cons value (cons prev next)))
(define (value-link link) (car link))
(define (prev-link link) (cadr link))
(define (next-link link) (cddr link))
(define (set-value-link! link value) (set-car! link value))
(define (set-prev-link! link prev) (set-car! (cdr link) prev))
(define (set-next-link! link next) (set-cdr! (cdr link) next))

;; Deque
(define (make-deque) (cons '() '()))
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-deque? deque) (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" (print-deque deque))
      (value-link (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" (print-deque deque))
      (value-link (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (cond ((empty-deque? deque)
         (let ((new-link (make-link item '() '())))
           (set-front-ptr! deque new-link)
           (set-rear-ptr! deque new-link)
           (print-deque deque)))
        (else
          (let ((front-link (front-ptr deque)))
            (let ((new-link (make-link item '() front-link)))
              (set-prev-link! front-link new-link)
              (set-front-ptr! deque new-link)
              (print-deque deque))))))

(define (rear-insert-deque! deque item)
  (cond ((empty-deque? deque)
         (let ((new-link (make-link item '() '())))
           (set-front-ptr! deque new-link)
           (set-rear-ptr! deque new-link)
           (print-deque deque)))
        (else
          (let ((rear-link (rear-ptr deque)))
            (let ((new-link (make-link item rear-link '())))
              (set-next-link! rear-link new-link)
              (set-rear-ptr! deque new-link)
              (print-deque deque))))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE! called with an empty deque" (print-deque deque)))
        ((eq? (front-ptr deque) (rear-ptr deque))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '())
         (print-deque deque))
        (else
          (let ((new-front-link (next-link (front-ptr deque))))
            (set-prev-link! new-front-link '())
            (set-front-ptr! deque new-front-link)
            (print-deque deque)))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "REAR-DELETE! called with an empty deque" (print-deque deque)))
        ((eq? (front-ptr deque) (rear-ptr deque))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '())
         (print-deque deque))
        (else
          (let ((new-rear-link (prev-link (rear-ptr deque))))
            (set-next-link! new-rear-link '())
            (set-rear-ptr! deque new-rear-link)
            (print-deque deque)))))

(define (print-deque deque)
  (define (print-iter link)
    (cond ((null? link)
           (display ""))
          (else
            (display (value-link link))
            (display " ")
            (print-iter (next-link link)))))
  (print-iter (front-ptr deque)))

;; Test
(define q1 (make-deque))

(display "Deque: ")
(print-deque q1)
(newline)

(newline)
(display "(front-insert-deque! q1 'a): ")
(front-insert-deque! q1 'a)
(newline)

(newline)
(display "(front-insert-deque! q1 'b): ")
(front-insert-deque! q1 'b)
(newline)

(newline)
(display "(rear-insert-deque! q1 'c): ")
(rear-insert-deque! q1 'c)
(newline)

(newline)
(display "(front-delete-deque! q1): ")
(front-delete-deque! q1)
(newline)

(newline)
(display "(rear-delete-deque! q1): ")
(rear-delete-deque! q1)
(newline)

(newline)
(display "(rear-delete-deque! q1): ")
(rear-delete-deque! q1)
(newline)

(newline)
(display "(rear-delete-deque! q1): ")
(rear-delete-deque! q1)
(newline)
