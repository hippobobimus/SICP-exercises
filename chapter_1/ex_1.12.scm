(define (pascal-element n)

    (define (func row col)
        (cond ((or (< row 1) (< col 1) (> col row)) 0)
              ((= row 1) 1)
              (else (+ (func (- row 1) (- col 1))
                       (func (- row 1) col)))))
        
    (define (get-row-col counter row-counter col-counter)
        (if (<= n counter)
            (func row-counter col-counter)
            (get-row-col (+ counter row-counter 1) (+ row-counter 1) (- n counter))))

    (get-row-col 1 1 1))
