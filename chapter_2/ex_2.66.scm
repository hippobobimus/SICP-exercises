#lang racket

;; Tree constructor and selectors
(define (make-tree entry left right)
  (list entry left right))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

;; Tree <> List conversion
(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))

;; Database
(define (make-record key data) (list key data))

(define (key record) (car record))

(define (data record) (cadr record))

(define (lookup given-key set-of-records)
  (let ((datum (entry set-of-records)))
    (cond ((null? set-of-records) #f)
          ((equal? given-key (key datum)) datum)
          ((< given-key (key datum))
           (lookup given-key (left-branch set-of-records)))
          ((< (key datum) given-key)
           (lookup given-key (right-branch set-of-records))))))

;; Test
(define rec1 (make-record 1 "Adam"))
(define rec2 (make-record 2 "Beau"))
(define rec3 (make-record 3 "Clive"))
(define rec4 (make-record 4 "Daisy"))
(define rec5 (make-record 5 "Earl"))
(define records (list->tree (list rec1 rec2 rec3 rec4 rec5)))

(display "Lookup record id 1: ")
(display (lookup 1 records))
(newline)
(display "Lookup record id 2: ")
(display (lookup 2 records))
(newline)
(display "Lookup record id 3: ")
(display (lookup 3 records))
(newline)
(display "Lookup record id 4: ")
(display (lookup 4 records))
(newline)
(display "Lookup record id 5: ")
(display (lookup 5 records))
(newline)
