; 561, 1105, 1729, 2465, 2821, 6601

(define (test-charmichael n)
  (define (try a)
    (cond ((= a n) #t)
	  ((= (expmod a n n) a) (try (+ a 1)))
	  (else #f)))
  (try 1))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp) 
	 (remainder (square (expmod base (/ exp 2) m)) 
		    m))
	(else 
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))
