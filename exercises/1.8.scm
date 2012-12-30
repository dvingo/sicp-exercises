(define (cube x)
  (* x x x))

(define (good-enough-cube? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (improve-cube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess))
     3.0))

(define (cubert-iter guess x)
  (if (good-enough-cube? guess x)
      guess
      (cubert-iter (improve-cube guess x) x)))

(define (cubert x)
  (cubert-iter 1.0 x))
