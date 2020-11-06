#lang racket

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (valid-password? p)
    (eq? password p))
  (define (dispatch p m)
    (cond ((not (valid-password? p))
           (lambda _ "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

;; TEST
(define acc (make-account 100 'secret-password))

(display "Withdraw 40: ")
(display ((acc 'secret-password 'withdraw) 40))
(newline)
(display "Deposit 50: ")
(display ((acc 'some-other-password 'deposit) 50))
(newline)
