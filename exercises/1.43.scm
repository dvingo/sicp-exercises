(define (repeated f n)
  (define (helper g a)
    (if (= a 1)
        g
	(compose g (helper g (- a 1)))))
  (lambda (x)
    ((helper f n) x)))

(repeated f 3)
(compose g (helper g 2))
(compose g (compose g (helper g 1)))
(compose g (compose g g))
