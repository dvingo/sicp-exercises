(define (my-equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b)))
	 (eq? a b))
	((and (list? a) (list? b))
	 (and (equal? (car a) (car b))
	      (equal? (cdr a) (cdr b))))
	 (else #f)))
	    

(my-equal? 'a 'b) ; #f
(my-equal? 'a 'a) ; #t
(my-equal? (list 'a) (list 'a)) ; #t
(my-equal? 'a (list 'b)); #f
(my-equal? 'a (list 'a)); #f
(my-equal? '(a b cd) '(a b cd)); #t
