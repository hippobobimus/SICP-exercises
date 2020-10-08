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
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-encoded-msg '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(define sample-decoded-msg '(A D A B B C A))
(define leaf-pairs '((A 4) (B 2) (C 1) (D 1)))
(define tree1 (generate-huffman-tree leaf-pairs))

(display "Leaf pairs: ")
(display leaf-pairs)
(newline)
(display "Huffman tree: ")
(display tree1)
(newline)
(display "Sample Huffman tree: ")
(display sample-tree)
(newline)
(display "Decoded message: ")
(display sample-decoded-msg)
(newline)
(display "Encoded message: ")
(display (encode sample-decoded-msg tree1))
(newline)
(display "Test: ")
(if (equal? (encode sample-decoded-msg tree1)
            sample-encoded-msg)
    (display "Passed.")
    (display "Failed."))
(newline)
