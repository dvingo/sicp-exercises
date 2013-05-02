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
; move origin to bottom left of screen
(graphics-set-coordinate-limits d 0 0 2 2)

;(define orig (make-vect 0 1))
;(define e1 (make-vect -1.3 -0.2))
;(define e2 (make-vect 0.1 0.1))
;(define f (make-frame orig e1 e2))


;(define orig (make-vect 0 0))
;(define f (make-frame orig (make-vect 0.5 0.5)
;		      (make-vect 0.2 0.5)))

;(define f2 (make-frame (make-vect -0.5 -0.5)
;		       (make-vect 0.9 0.9)
;		       (make-vect 0.3 0.7)))

;(define f3 (make-frame (make-vect 0 0)
;		       (make-vect 0 1)
;		       (make-vect 1 0)))
		       
		      
;(s-painter f d)

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
  
; b:
(define (cross-frame f graph-dev)
  (define bl (origin-frame f))
  (define tl (edge1-frame f))
  (define tr (add-vect (edge2-frame f) tl))
  (define br (edge2-frame f))
  (define seg1 (make-segment bl tr))
  (define seg2 (make-segment br tl))
  (define cross-painter (segments->painter (list seg1 seg2)))
  (cross-painter f graph-dev))

; c:
(define (midpoint-segment line-segment)
  (let ((x1 (xcor-vect (start-segment line-segment)))
	(x2 (xcor-vect (end-segment line-segment)))
	(y1 (ycor-vect (start-segment line-segment)))
	(y2 (ycor-vect (end-segment line-segment))))
	(make-vect
	 (/ (+ x1 x2) 2)
	 (/ (+ y1 y2) 2))))
(define (diamond-frame f graph-dev)
  (let ((bl (origin-frame f))
	(tl (edge1-frame f))
	(tr (add-vect (edge2-frame f) (edge1-frame f)))
	(br (edge2-frame f)))
    (let ((seg1 (make-segment bl tl))
	  (seg2 (make-segment tl tr))
	  (seg3 (make-segment tr br))
	  (seg4 (make-segment br bl)))
      (let ((diam-bl (midpoint-segment seg1))
	    (diam-tl (midpoint-segment seg2))
	    (diam-tr (midpoint-segment seg3))
	    (diam-br (midpoint-segment seg4)))
	(let ((diam-seg1 (make-segment diam-bl diam-tl))
	      (diam-seg2 (make-segment diam-tl diam-tr))
	      (diam-seg3 (make-segment diam-tr diam-br))
	      (diam-seg4 (make-segment diam-br diam-bl)))
	  (let ((diamond-painter (segments->painter (list diam-seg1
							 diam-seg2
							 diam-seg3
							 diam-seg4))))
	    (diamond-painter f graph-dev)))))))
; (diamond-frame f2 d)

; alternative diamond painter:
(define (diamond-f f graph-dev)
  (let ((segs (list (make-segment (make-vect 0 0.5)
				  (make-vect 0.5 0))
		    (make-segment (make-vect 0.5 0)
				  (make-vect 1 0.5))
		    (make-segment (make-vect 0.5 1)
				  (make-vect 1 0.5))
		    (make-segment (make-vect 0.5 1)
				  (make-vect 0 0.5)))))
      ((segments->painter segs) f graph-dev)))

; d:
; for some reason the coordinates are now reversed, make-vect 
; should be viewed as (y x) not (x y)
(define (wave-frame f graph-dev)
  (define guy (list (make-segment (make-vect 0 0.4)
				  (make-vect 0.33 0.5))
		    (make-segment (make-vect 0.33 0.5)
				  (make-vect 0 0.6))
		    ; left side
		    (make-segment (make-vect 0 0.33)
				  (make-vect 0.6 0.4))
		    ; left arm
		    (make-segment (make-vect 0.6 0.4)
				  (make-vect 0.5 0.2))
		    (make-segment (make-vect 0.5 0.2)
				  (make-vect 0.8 0))

		    (make-segment (make-vect 0.7 0.4)
				  (make-vect 0.6 0.2))
		    (make-segment (make-vect 0.6 0.2)
				  (make-vect 0.9 0))

		    ; neck
		    (make-segment (make-vect 0.7 0.4)
				  (make-vect 0.7 0.45))
		    (make-segment (make-vect 0.7 0.55)
				  (make-vect 0.7 0.6))
		    (make-segment (make-vect 0.7 0.45)
				  (make-vect 0.75 0.45))
		    (make-segment (make-vect 0.7 0.55)
				  (make-vect 0.75 0.55))

		    ; head
		    (make-segment (make-vect 0.75 0.45)
				  (make-vect 0.8 0.4))
		    (make-segment (make-vect 0.75 0.55)
				  (make-vect 0.8 0.60))

		    (make-segment (make-vect 0.8 0.4)
				  (make-vect 0.85 0.4))
		    (make-segment (make-vect 0.8 0.6)
				  (make-vect 0.85 0.6))
		    
		    (make-segment (make-vect 0.85 0.4)
				  (make-vect 1.00 0.5))
		    (make-segment (make-vect 0.85 0.6)
				  (make-vect 1.00 0.5))

		    ; right side
		    (make-segment (make-vect 0 0.67)
				  (make-vect 0.6 0.6))
		    (make-segment (make-vect 0.6 0.6)
				  (make-vect 0.2 1))
		    (make-segment (make-vect 0.7 0.6)
				  (make-vect 0.25 1))))
  ((segments->painter guy) f graph-dev))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Graphics test code for any future reference needs:
;(define d (make-graphics-device (car (enumerate-graphics-types))))
;(graphics-operation d 'set-foreground-color "red")
;(graphics-operation d 'set-background-color "blue") 
;(graphics-operation d 'fill-circle 0 0 0.1)
;(graphics-draw-line d 0 0 5 5)
;(graphics-close d)

; Drawing in MIT-Scheme References:
;http://sicp.ai.mit.edu/Spring-2005/manuals/scheme-7.5.5/doc/scheme_18.html#SEC192
; http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Graphics.html#Graphics

; to display the current screen coordinates:
;(call-with-values  (lambda () (graphics-coordinate-limits d) )
;  (lambda (x y a b)
;    (display x)
;    (display y) 
;    (display a)
;    (display b)))

; to change the virtual coordinate system of the opened window (device)
;(graphics-set-coordinate-limits d 0 0 2 2)
;(graphics-set-coordinate-limits d 0 0 1 1)
