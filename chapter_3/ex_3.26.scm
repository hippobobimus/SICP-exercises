#lang racket
(require racket/trace)
(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

;; Tree and datum representations
(define (make-tree datum left right)
  (list datum left right))
(define (datum tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-datum key data)
  (mcons key data))
(define (key datum) (mcar datum))
(define (data datum) (mcdr datum))
(define (set-data! datum value) (set-mcdr! datum value))

;; Table constructor
(define (make-table)
  (let ((local-table (make-datum '*table* '())))
    (define (lookup keys)
      (define (helper keys current-datum)
        (if (null? keys)
            (data current-datum)
            (let ((subdatum (assoc (car keys) (data current-datum))))
              (if subdatum
                  (helper (cdr keys) subdatum)
                  #f))))
      (helper keys local-table))

    (define (insert! keys value)
      (define (helper keys current-datum)
        (if (null? keys)
            (set-data! current-datum value)
            (let ((subdatum (assoc (car keys) (data current-datum))))
              (if subdatum
                  (helper (cdr keys) subdatum)
                  (begin
                    (set-data! current-datum
                               (adjoin-tree (make-datum (car keys) '())
                                            (data current-datum)))
                    (helper (cdr keys) (assoc (car keys) (data current-datum))))))))
      (helper keys local-table)
      'ok)

    (define (adjoin-tree new-datum tree)
      (cond ((null? tree) (make-tree new-datum '() '()))
            ((symbol<? (key new-datum) (key (datum tree)))
             (make-tree (datum tree)
                        (adjoin-tree new-datum (left-branch tree))
                        (right-branch tree)))
            ((symbol<? (key (datum tree)) (key new-datum))
             (make-tree (datum tree)
                        (left-branch tree)
                        (adjoin-tree new-datum (right-branch tree))))))

    (define (assoc given-key tree)
      (if (null? tree)
          #f
          (let ((d (datum tree)))
            (cond ((equal? given-key (key d)) d)
                  ((symbol<? given-key (key d))
                   (assoc given-key (left-branch tree)))
                  ((symbol<? (key d) given-key)
                   (assoc given-key (right-branch tree)))))))

    (define (print)
      (display local-table)
      (newline))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print) print)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define print-table (operation-table 'print))

;; TESTS
(newline)
(print-table)
(newline)
(display "Find raven in local table: ")
(display (get '(birds raven)))
(newline)
(newline)
(display "Add animals-birds: ")
(newline)
(put '(animals birds raven) 3)
(put '(animals birds magpie) 2)
(put '(animals birds woodpecker) 4)
(put '(animals birds cuckoo) 1)
(newline)
(display "Add animals-marsupials: ")
(newline)
(put '(animals marsupials koala) 102)
(put '(animals marsupials bilby) 332)
(put '(animals marsupials wombat) 721)
(newline)
(display "Add vegetables: ")
(newline)
(put '(vegetables mushroom) 88)
(put '(vegetables carrot) 27)
(put '(vegetables zucchini) 92)
(newline)
(display "Find raven value: ")
(display (get '(animals birds raven)))
(newline)
(display "Find magpie value: ")
(display (get '(animals birds magpie)))
(newline)
(display "Change magpie value to 12: ")
(put '(animals birds magpie) 12)
(display "Find magpie value: ")
(display (get '(animals birds magpie)))
(newline)
(display "Find koala value: ")
(display (get '(animals marsupials koala)))
(newline)
(display "Find carrot value: ")
(display (get '(vegetables carrot)))
(newline)
(newline)
(print-table)
(newline)
