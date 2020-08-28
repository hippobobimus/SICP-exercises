;; Constructors
(define (make-mobile-old left right)
  (list left right))

(define (make-branch-old length structure)
  (list length structure))

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

;; Selectors
(define left-branch car)
(define (right-branch-old mobile) (car (cdr mobile)))
(define right-branch cdr)

(define branch-length car)
(define (branch-structure-old branch) (car (cdr branch)))
(define branch-structure cdr)

;; Procedures
(define (total-weight mobile)
  (cond ((null? mobile)
         0)
        ((not (pair? mobile))
         mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

(define (balanced? mobile)
  (define (torque branch)
    (* (branch-length branch)
       (total-weight (branch-structure branch))))
  (cond ((pair? mobile)
         (let ((lb (left-branch mobile))
               (rb (right-branch mobile)))
           (and (equal? (torque lb)
                        (torque rb))
                (balanced? (branch-structure lb))
                (balanced? (branch-structure rb)))))
        (else #t)))

;; Test
(define left (make-branch 3 7))
(define left (make-branch 3 6))
(define right (make-branch 1 9))
(define right (make-branch 2 9))
(define mob-1 (make-mobile left right))

(define mob (make-mobile (make-branch 3 mob-1)
                         (make-branch 3 15)))

(newline)
(display "Mobile: ")
(display mob)
(newline)
(display "Total-weight: ")
(display (total-weight mob))
(newline)
(display "Balanced?: ")
(display (balanced? mob))
(newline)
