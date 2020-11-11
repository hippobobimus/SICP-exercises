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
  (define (get-balance)
    balance)
  (define (valid-password? p)
    (eq? password p))
  (define (dispatch p m)
    (cond ((not (valid-password? p))
           (lambda _ "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'balance) get-balance)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define (make-joint account original-password new-password)
  (lambda (p m)
    (if (eq? p new-password)
        (account original-password m)
        (lambda _ "Incorrect password"))))

;; TEST
(define peter-acc (make-account 100 'open-sesame))
(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

(display "Peter withdraw 40: ")
(display ((peter-acc 'open-sesame 'withdraw) 40))
(newline)
(display "Paul deposit 50: ")
(display ((paul-acc 'rosebud 'deposit) 50))
(newline)
(display "Peter balance: ")
(display ((peter-acc 'open-sesame 'balance)))
(newline)
(display "Paul balance: ")
(display ((paul-acc 'rosebud 'balance)))
(newline)
(display "Paul balance with wrong password: ")
(display ((paul-acc 'open-sesame 'balance)))
(newline)
