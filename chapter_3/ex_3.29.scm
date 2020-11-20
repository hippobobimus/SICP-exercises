#lang sicp

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((x (make-wire))
          (y (make-wire))
          (z (make-wire)))
      (inverter a1 x)
      (inverter a2 y)
      (and-gate x y z)
      (inverter z output)))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
