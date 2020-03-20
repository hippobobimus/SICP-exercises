(define (func-iter a b c counter)
    (if (< counter 3)
        a
        (func-iter (+ a (* 2 b) (* 3 c)) a b (- counter 1))))

(define (func n)
    (if (< n 3)
        n
        (func-iter 2 1 0 n)))
