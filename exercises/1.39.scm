(define (cont-frac-sub n d k)
  (define (iter i holder)
    (if (= i k)
	holder
	(iter (+ i 1) (/ (n (- k i)) (- (d (- k i)) holder)))))
  (iter 1 (/ (n k) (d k))))

(define (odd? i) (= (remainder i 2) 1))

(define (tan-cf x k)
  (cont-frac-sub (lambda (i)
		   (if (= i 1) x (* x x)))
	       (lambda (i) (+ i (- i 1)))
	       k))
(tan-cf 3.1415926535 1000) ; == 0

; pi / 6 = 0.52359877
(tan-cf 0.52359877 1000) ; tan(pi/6) == 0.5773502 == sqrt(3)/3

; page 110 avg damping
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10) ; 55
