#lang sicp
(#%require "constraints.scm")

;; EXERCISE
(define (averager a b average)
  (let ((x (make-connector))
        (y (make-connector)))
    (adder a b y)
    (multiplier average x y)
    (constant 2 x)
    'ok))

;; TEST
(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(averager a b c)

(probe "a" a)
(probe "b" b)
(probe "c" c)

(set-value! a 2 'user)
(set-value! b 4 'user)

(forget-value! b 'user)
(set-value! b 12 'user)
