#lang racket

(define (rand-update x) (+ x 1))
(define random-init 0)

(define rand
  (let ((x random-init))
    (define (dispatch msg)
      (cond ((eq? msg 'generate)
             (set! x (rand-update x))
             x)
            ((eq? msg 'reset)
             (lambda (new-init)
               (set! x new-init)
               'ok))))
    dispatch))

;; TEST
(display (rand 'generate))
(newline)
(display (rand 'generate))
(newline)
(display (rand 'generate))
(newline)
(display (rand 'generate))
(newline)
(display "RESET: ")
(display ((rand 'reset) 9))
(newline)
(display (rand 'generate))
(newline)
(display (rand 'generate))
(newline)
(display (rand 'generate))
(newline)

