(define (gold x)
  (fixed-point (lambda (y) (+ 1 (/ 1 y))) 1.0))
