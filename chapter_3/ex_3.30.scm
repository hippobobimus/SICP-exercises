#lang sicp

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder a-list b-list s-list c)
  (let ((c-list (map (lambda (_) (make-wire))       ;; c-list is c1 to cn-1
                     a-list)))
    (let ((c-in-list (append c-list (make-wire)))
          (c-out-list (cons c c-list)))
      (map full-adder
           a-list
           b-list
           c-in-list
           s-list
           c-out-list)
      'ok)))


