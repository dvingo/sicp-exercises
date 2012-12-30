; linear recursive (logarithmic steps):
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(fast-expt 2 8)
(square (fast-expt 2 4))
(square (square (fast-expt 2 2)))
(square (square (square (fast-expt 2 1))))
(square (square (square (* 2 (fast-expt 2 0)))))
(square (square (square (* 2 1))))
(square (square (square 2)))
(square (square 4))
(square 16)
256

; iterative algorithm:
(define (even? n)
  (= (remainder n 2) 0))
(define (square n) (* n n))
(define (exp b n)
  (it-exp b n 1))
(define (it-exp b n a)
  (cond ((= n 2) (* a (square b)))
	((even? n) (it-exp b (/ n 2) (square b)))
	(else (* b (it-exp b (- n 1) a)))))
