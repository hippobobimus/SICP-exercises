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

;; Test painter draws a right angle triangle on the left side of the frame
(define painter-test (segments->painter (list (make-segment 0.0 0.0 0.5 0.0)
                                              (make-segment 0.0 0.0 0.5 1.0)
                                              (make-segment 0.5 0.0 0.5 1.0))))

;; Painter transformation
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
          (make-frame new-origin
                      (sub-vect (m corner1) new-origin)
                      (sub-vect (m corner2) new-origin)))))))
;; Exercise
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeat func n)
  (if (= n 1)
      func
      (compose func (repeat func (- n 1)))))

(define (rotate180 painter)
  ((repeat rotate90 2) painter))

(define (rotate270 painter)
  ((repeat rotate90 3) painter))

;; Test
(define frm (make-frame (make-vect 100 100)
                        (make-vect 300 0)
                        (make-vect 0 300)))

;;((flip-horiz painter-test) frm)
;;((rotate180 painter-test) frm)
((rotate270 painter-test) frm)
