; (load "2.8.scm")

(define (width interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))

(define x (make-interval 1.3 3.4))
(define y (make-interval 2.3 5.8))

(width x) ; 1.0499
(width y) ; 1.75
(add-interval x y) ; (3.5999999999999996 . 9.2)
(width (add-interval x y)) ; 2.8
(width (mul-interval x y)) ; 8.365
