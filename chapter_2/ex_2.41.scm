;; Required helper functions
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (enumerate-interval start end)
  (if (> start end)
      '()
      (cons start (enumerate-interval (+ start 1) end))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

;; Exercise
(define (make-triple-sum triple)
  (list (car triple)
        (cadr triple)
        (caddr triple)
        (+ (car triple) (cadr triple) (caddr triple))))

(define (unique-triples n)
  (flatmap
    (lambda (i)
      (flatmap
        (lambda (j)
          (map (lambda (k) (list i j k))
               (enumerate-interval 1 (- j 1))))
        (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (find-triples-for-sum n s)
  (define (sum-equals-s? triple)
    (= (+ (car triple) (cadr triple) (caddr triple))
       s))
  (map make-triple-sum
       (filter sum-equals-s?
               (unique-triples n))))

;; Test
(newline)
(display "Unique triples, n=5: ")
(display (unique-triples 5))
(newline)
(display "Triples that sum to 7: ")
(display (find-triples-for-sum 5 7))

