#lang sicp
(#%require sicp-pict)

(define (split parent child)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split parent child) painter (- n 1))))
          (parent painter (child smaller smaller))))))

(define right-split (split beside below))

(define up-split (split below beside))

;; Test
(display "Right-split")
(newline)
(paint (right-split einstein 3))
(newline)
(display "Up-split")
(newline)
(paint (up-split einstein 3))