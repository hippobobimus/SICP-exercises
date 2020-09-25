#lang racket

(require graphics/graphics)
(open-graphics)

;; Set size of canvas
(define vp-size 500)

;; Viewport
(define vp (open-viewport "A Picture Language" vp-size vp-size))

(define draw (draw-viewport vp))

(define (clear) ((clear-viewport vp)))

;; Graphics package uses posn not vect as well as a coord system inverted in the y-axis,
;; Therefore, add conversion wrapper to enable interaction with our code.
(define (vector-to-posn v)
  (make-posn (xcor-vect v) (- vp-size (ycor-vect v))))

;; Draw line function to be used by our code
(define (draw-line-new v1 v2)
  ((draw-line vp) (vector-to-posn v1)
                  (vector-to-posn v2)))

;; Frame constructor and selectors
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame car)

(define edge1-frame cadr)

(define edge2-frame caddr)

;; Frame coord map
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))

;; Vector constructor and selectors
(define make-vect cons)

(define xcor-vect car)

(define ycor-vect cdr)

;; Vector procedures
(define (add-vect v w)
  (make-vect (+ (xcor-vect v)
                (xcor-vect w))
             (+ (ycor-vect v)
                (ycor-vect w))))

(define (sub-vect v w)
  (make-vect (- (xcor-vect v)
                (xcor-vect w))
             (- (ycor-vect v)
                (ycor-vect w))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;; Segment constructor and selectors
(define (make-segment x0 y0 x1 y1)
  (cons (make-vect x0 y0)
        (make-vect x1 y1)))

(define start-segment car)

(define end-segment cdr)

;; Painter constructor
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line-new
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

;; (a) Draws the outline of the frame
(define painter-a (segments->painter (list (make-segment 0 0 0 1)
                                           (make-segment 0 0 1 0)
                                           (make-segment 1 0 1 1)
                                           (make-segment 0 1 1 1))))

;; (b) Draws an X by connecting opposite corners
(define painter-b (segments->painter (list (make-segment 0 0 1 1)
                                           (make-segment 1 0 0 1))))

;; (c) Draws a diamond by connecting midpoints of frame sides
(define painter-c (segments->painter (list (make-segment 0.5 0 1 0.5)
                                           (make-segment 0.5 0 0 0.5)
                                           (make-segment 0 0.5 0.5 1)
                                           (make-segment 1 0.5 0.5 1))))

;; Test
(define frm (make-frame (make-vect 100 100)
                        (make-vect 300 0)
                        (make-vect 0 300)))
(painter-a frm)
(painter-b frm)
(painter-c frm)
