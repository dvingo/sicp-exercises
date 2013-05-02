(load "2.50.scm")
; from the text:
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame graphic-dev)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter
	 (make-frame new-origin
		     (sub-vect (m corner1) new-origin)
		     (sub-vect (m corner2) new-origin))
	 graphic-dev)))))

; from the text:
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-left
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      split-point
			      (make-vect 1.0 0.0)))
	  (paint-right
	   (transform-painter painter2
			      split-point
			      (make-vect 0.0 1.0)
			      (make-vect 1.0 0.5))))
      (lambda (frame graphic-dev)
	(paint-left frame graphic-dev)
	(paint-right frame graphic-dev)))))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      split-point
			      (make-vect 0.0 1.0)))
	  (paint-right
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.0)
			      (make-vect 0.5 1.0))))
      (lambda (frame graphic-dev)
	(paint-left frame graphic-dev)
	(paint-right frame graphic-dev)))))


(define (below2 painter1 painter2)
  (flip-horiz (rotccw-270 (beside painter1 painter2))))
