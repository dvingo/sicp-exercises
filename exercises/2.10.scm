; (load "2.9.scm")
(define (div-interval x y)
  (if (or (= (upper-bound y) 0)
	  (= (lower-bound y) 0)
	  (and (> (upper-bound y) 0)
	       (< (lower-bound y) 0)))
      (error "attempted division by zero!")
      (mul-interval x
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))

; (div-interval (make-interval 0 2) x) ; works
; (div-interval x (make-interval -1.2 2)) ; fail!
; (div-interval x (make-interval 0 2)) ; fail!
; (div-interval x (make-interval 0.1 0)) ; fail!
