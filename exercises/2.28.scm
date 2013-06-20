(define (fringe l)
  (define (helper li ans)
    (cond ((not (list? li)) (append ans (list li)))
	  ((null? (cdr li)) (helper (car li) ans))
	  (else
	   (append (helper (car li) ans) (helper (cdr li) ans)))))
(helper l ()))
