#lang sicp
(#%require "constraints.scm")

;; EXERCISE
(define (squarer a b)
  (multiplier a a b))

;; TEST
(define a (make-connector))
(define b (make-connector))

(squarer a b)

(probe "a" a)
(probe "b" b)

;;(set-value! a 2 'user)
(set-value! b 4 'user)
