;; Coin denominations
(define us-coins (list 50 25 10 5 1))
(define us-coins (list 50 5 25 10 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define uk-coins (list 100 50 5 10 20 2 1 0.5))

;; Count ways of making change algorithm
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values)))))

(define first-denomination car)

(define except-first-denomination cdr)

(define no-more? null?)

;; Test
(define ways-us (cc 100 us-coins))
(define ways-uk (cc 100 uk-coins))

(newline)
(display "Ways to change 100 with US coins: ")
(display ways-us)
(newline)
(display "Ways to change 100 with UK coins: ")
(display ways-uk)
(newline)
