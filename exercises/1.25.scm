; from the text:
(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))
The difference lies in the fact that we take the remainder of the successive square or
mulitplication each time through the recursion, and thus we are dealing with
numbers that must be smaller than m each time through the recursion.  Whereas
In fast-expt we don't use this trick.
