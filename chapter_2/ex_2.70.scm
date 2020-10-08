#lang racket

;; Leaf
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

;; Tree
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; Decoding
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
                (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; Encoding
(define (symbol-in-set? symbol set)
  (cond ((null? set) #f)
        ((eq? symbol (car set)) #t)
        (else (symbol-in-set? symbol (cdr set)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      '()
      (let ((left (left-branch tree))
            (right (right-branch tree)))
        (cond ((symbol-in-set? symbol (symbols left))
               (cons 0 (encode-symbol symbol left)))
              ((symbol-in-set? symbol (symbols right))
               (cons 1 (encode-symbol symbol right)))
              (else (error "bad symbol -- ENCODE-SYMBOL" symbol))))))

;; Generating a Huffman encoding tree
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (successive-merge (adjoin-set (make-code-tree (car leaf-set)
                                                    (cadr leaf-set))
                                    (cddr leaf-set)))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; Test
(define leaf-pairs '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))
(define tree1 (generate-huffman-tree leaf-pairs))
(define message '(GET A JOB
                      SHA NA NA NA NA NA NA NA NA
                      GET A JOB
                      SHA NA NA NA NA NA NA NA NA
                      WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                      SHA BOOM))

(display "Leaf pairs: ")
(display leaf-pairs)
(newline)
(display "Huffman tree: ")
(display tree1)
(newline)
(display "Message: ")
(display message)
(newline)
(display "Length of message (symbols): ")
(display (length message))
(newline)
(display "Decoded message: ")
(display (encode message tree1))
(newline)
(display "Length of encoded message (bits): ")
(display (length (encode message tree1)))
(newline)
(display "Length of original message (symbols): ")
(display (length message))
(newline)
(display "Using a fixed-length code would require 3 bits per symbol for the 8-symbol alphabet.")
(newline)
(display "Therefore the smallest number of bits required to encode the song in this way would be: ")
(display (* 3 (length message)))
(newline)
