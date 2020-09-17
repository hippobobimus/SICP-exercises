(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff
                   (* x higher-terms)))
              0
              coefficient-sequence))

;; Test
(define coeff-seq (list 1 3 0 5 0 1))
(newline)
(display "Evaluate 1 + 3x + 5x^3 + x^5 at x = 2...")
(newline)
(display "Result: ")
(display (horner-eval 2 coeff-seq))
