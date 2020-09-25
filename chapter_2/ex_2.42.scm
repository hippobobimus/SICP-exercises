;;Support functions
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

;; Board and position functions
(define (make-position row col)
  (list row col))

(define row car)

(define col cadr)

(define empty-board '())

(define (adjoin-position row col positions)
  (cons (make-position row col)
        positions))

;; Check whether a queen's position is safe
(define (safe? queen-col positions)
  (let ((queen-row (row (car (filter (lambda (x) (= (col x) queen-col))
                                     positions)))))
    (define (iter up-diag down-diag rest-of-positions)
      (if (null? rest-of-positions)
          #t
          (let ((test-queen-row (row (car rest-of-positions))))
            (if (or (= queen-row test-queen-row)
                    (= up-diag test-queen-row)
                    (= down-diag test-queen-row))
                #f
                (iter (- up-diag 1) (+ down-diag 1) (cdr rest-of-positions))))))
    (iter (- queen-row 1) (+ queen-row 1) (cdr positions))))

;; Produces all the ways of safely placing n queens on an n x n chess board
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

;; Tests
(define board-1 (list (list 3 1)
                      (list 7 2)
                      (list 2 3)
                      (list 8 4)
                      (list 5 5)
                      (list 1 6)
                      (list 4 7)
                      (list 6 8)))

(newline)
(display "Ways to place 8 queens: ")
(define ways (queens 8))
(display (length ways))
(newline)
(display "List of valid board layouts: ")
(newline)
(display (queens 8))
