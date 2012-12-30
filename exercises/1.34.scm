f will be bound to the formal parameter g and then will attempt
to have 2 applied to it.  As 2 is not a function, you cannot apply
2 to 2, resulting in a runtime error.

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((positive? test-value)
		 (search f neg-point midpoint))
		((negative? test-value)
		 (search f midpoint pos-point))
		(else midpoint))))))
(define (average x y) (/ (+ x y) 2))
(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b))
	  ((and (negative? b-value) (positive? a-value))
	   (search f b a))
	  (else
	   (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)


; finding the fixed point of cos
scheme@(guile-user)> (cos 1)
$6 = 0.54030230586814
scheme@(guile-user)> (cos 0.54030230586814)
$7 = 0.857553215846393
scheme@(guile-user)> (cos 0.857553215846393)
$8 = 0.654289790497779
scheme@(guile-user)> (cos 0.654289790497779)
$9 = 0.793480358742566
scheme@(guile-user)> (cos 0.793480358742566)
$10 = 0.701368773622756
scheme@(guile-user)> (cos 0.701368773622756)
$11 = 0.763959682900655
scheme@(guile-user)> (cos 0.763959682900655)
$12 = 0.722102425026707
scheme@(guile-user)> (cos 0.722102425026707)
$13 = 0.750417761763761
scheme@(guile-user)> (cos 0.750417761763761)
$14 = 0.73140404242251
scheme@(guile-user)> (cos 0.73140404242251)
$15 = 0.744237354900557
scheme@(guile-user)> (cos 0.744237354900557)
$16 = 0.735604740436347
scheme@(guile-user)> (cos 0.735604740436347)
$17 = 0.741425086610109
scheme@(guile-user)> (cos 0.741425086610109)
$18 = 0.737506890513243
scheme@(guile-user)> (cos 0.737506890513243)
$19 = 0.740147335567876
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (/ x y)) 1.0))
