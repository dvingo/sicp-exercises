(load "2.46.scm") ; vector functions

; frame code from 2.47:

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cddr frame))

; test code: 
;(define orig (make-vect 0 1))
;(define e1 (make-vect 1.3 2.2))
;(define e2 (make-vect 0.1 0.2))
;(define f (make-frame orig e1 e2))
;(origin-frame f)
;(edge1-frame f)
;(edge2-frame f)

; from the text pg 187:
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
			   (edge1-frame frame))
	       (scale-vect (ycor-vect v)
			   (edge2-frame frame))))))

;(define frame-mapper (frame-coord-map f))
;(frame-mapper orig)
;(frame-mapper e1)
;(frame-mapper e2)

;; From the text:
(define (for-each f items)
  (define (helper i)
    (f i)
    (for-each f (cdr items)))
  (if (null? items)
      #t
      (helper (car items))))

(define (segments->painter segment-list)
  (lambda (frame graph-device)
    (for-each
     (lambda (segment)
       (display "drawing segment: ")(display segment) (newline)
       (display "frame-mapped start: ")
       (display ((frame-coord-map frame) (start-segment segment)))
       (display "frame-mapped end: ")
       (display ((frame-coord-map frame) (end-segment segment)))
       (draw-line
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))
	graph-device))
     segment-list)))

; segment code
(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (draw-line v1 v2 d)
  (graphics-draw-line d (xcor-vect v1) (ycor-vect v1)
		      (xcor-vect v2) (ycor-vect v2)))


(define d (make-graphics-device (car (enumerate-graphics-types))))
;(define v1 (make-vect 0 0))
;(define v2 (make-vect 0.5 0.3))
;(define seg1 (make-segment v1 v2))
;(draw-line (start-segment seg1) (end-segment seg1) d)

; finally to the code:
(define seg1 (make-segment (make-vect 0.1 0)
			   (make-vect 0.3 0)))
(define seg2 (make-segment (make-vect 0.2 0.1)
			   (make-vect 0.8 0.8)))

(define seg-list (list seg1 seg2))

(define s-painter (segments->painter seg-list))

;(define orig (make-vect 0 1))
;(define e1 (make-vect -1.3 -0.2))
;(define e2 (make-vect 0.1 0.1))
;(define f (make-frame orig e1 e2))


(define orig (make-vect 0 0))
;(define e1 (make-vect -1.3 -0.2))
;(define e2 (make-vect 0.1 0.1)

(define f (make-frame orig (make-vect -0.5 -0.5)
		      (make-vect 0.2 0.5)))
		      
(s-painter f d)

; a:
(define (outline-frame f graph-dev)
  (define bl (origin-frame f))
  (define tl (edge1-frame f))
  (define tr (add-vect (edge2-frame f) tl))
  (define br (edge2-frame f))
  (define seg1 (make-segment bl tl))
  (define seg2 (make-segment tl tr))
  (define seg3 (make-segment tr br))
  (define seg4 (make-segment br bl))
  (define seg-list (list seg1 seg2 seg3 seg4))
  (define frame-painter (segments->painter seg-list))
  (frame-painter f graph-dev))
  

;(define f (make-frame orig (make-vect -0.5 -0.5)
;		      (make-vect 0.2 0.5)))

;(define f (make-frame orig (make-vect -0.9 -0.5)
;		      (make-vect 0.2 0.5)))

;(outline-frame f d)

;;;;;;;;;;;;;;;;;;;;;;;;;
; Graphics test code
(define d (make-graphics-device (car (enumerate-graphics-types))))
(graphics-operation d 'set-foreground-color "red")
(graphics-operation d 'set-background-color "blue") 
(graphics-operation d 'fill-circle 0 0 0.1)
(graphics-draw-line d 0 0 5 5)
(graphics-close d)

; Drawing in MIT-Scheme References:
;http://sicp.ai.mit.edu/Spring-2005/manuals/scheme-7.5.5/doc/scheme_18.html#SEC192
; http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Graphics.html#Graphics
