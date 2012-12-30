; iterative process of multiplication
(define (iter* a b c)
  (cond ((= b 1) c)
        ((even? b) (iter* a (halve b) (+ (double a) c)))
	(else (iter* a (- b 1) (+ a c)))))
(define (it* a b)
  (iter* a b 0))

(it* 3 4)
(iter* 3 4 0)
(iter* 3 2 (+ 6 0))
(iter* 3 1 (+ 6 6))
12


(it* 2 5)
(iter* 2 5 0)
(iter* 2 4 2)
(iter* 2 2 (+ 4 2))
(iter* 2 1 (+ 4 6))
10
