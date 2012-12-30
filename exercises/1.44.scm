(define (smooth f)
  (define dx 0.00001)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (g x)
  (+ x 2.0))

 (/ (+ (g (- 3.1 dx)) (g 3.1) (g (+ 3.1 dx))) 3.0)))

((repeated (smooth g) 2) 3.1)
(define (n-smooth f n)
    (repeated (smooth f) n))

((n-smooth sin 2) (/ pi 2))
((n-smooth sin 3) (/ pi 2))
((n-smooth sin 4) (/ pi 2))
((n-smooth sin 5) (/ pi 2))
((n-smooth sin 6) (/ pi 2))
((n-smooth sin 10) (/ pi 2))
