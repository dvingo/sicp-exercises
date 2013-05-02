; a
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
		    ; top of head
		    (make-segment (make-vect 0.85 0.4)
				  (make-vect 1.00 0.5))
		    (make-segment (make-vect 0.85 0.6)
				  (make-vect 1.00 0.5))

		    ; smile
		    (make-segment (make-vect 0.8 .45)
				  (make-vect 0.8 .5))

					; right side
		    (make-segment (make-vect 0 0.67)
				  (make-vect 0.6 0.6))
		    (make-segment (make-vect 0.6 0.6)
				  (make-vect 0.2 1))
		    (make-segment (make-vect 0.7 0.6)
				  (make-vect 0.25 1))))
  ((segments->painter guy) f graph-dev))

;b 
; from 2.44:
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

; after loading 2.49.scm:
; ((corner-split wave-frame 3) f3 d)
(define (corner-split2 painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((corner (corner-split painter (- n 1))))
          (beside (below painter up)
                  (below right corner))))))

;c
;from the text:
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-limit2 painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-vert quarter) quarter)))
      (below (flip-horiz half) half))))
