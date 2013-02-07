; exercise 2.2
(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment line-segment)
  (let ((x1 (x-point (start-segment line-segment)))
	(x2 (x-point (end-segment line-segment)))
	(y1 (y-point (start-segment line-segment)))
	(y2 (y-point (end-segment line-segment))))
	(make-point 
	 (/ (+ x1 x2) 2)
	 (/ (+ y1 y2) 2))))

(define (print-point p)
         (newline)
         (display "(")
         (display (x-point p))
         (display ",")
         (display (y-point p))
         (display ")"))

(define p1 (make-point 4 4))
(define p2 (make-point 8 8))

(define seg (make-segment (make-point 4 4) (make-point 8 8)))

;(print-point (make-point (/ (+ 4 8) 2) (/ (+ 4 8) 2)))
