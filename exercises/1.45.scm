(define (average x y) (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))
(define tolerance 0.0000001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display "guess: ")
    (display guess) (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

; does not converge:
(define (sqrt x)
      (fixed-point (lambda (y) (/ x y)) 1.0))

; does converge with one level of avg damping:
(define (sqrt x)
      (fixed-point (average-damp (lambda (y) (/ x y)))
1.0))
(define (cbrt x)
      (fixed-point (average-damp (lambda (y) (/ x (expt y 2))))
1.0))
; depends upon repeated and compose
(define (four-rt x)
    (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (expt y 3))))
1.0))

(define (five-rt x)
    (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (expt y 4))))
1.0))
  

(define (nth-rt n x num-avg)
    (fixed-point ((repeated average-damp num-avg) (lambda (y) (/ x (expt y (- n 1)))))
1.0))

; fourth root requires 2 average damps
; fifth root requires 2 average damps
; sixth root converges at 2
; seventh root converges at 2
; 8th root requires 3
; 9th root requires 3
; 10th root requires 3
; 11,12,13 3
; 16 requires 4

; number of average damps needed is:
; (floor (sqrt n))

(define (nth-rt n x)
    (fixed-point ((repeated average-damp(floor (sqrt n)))
		  (lambda (y) (/ x (expt y (- n 1)))))
1.0))
