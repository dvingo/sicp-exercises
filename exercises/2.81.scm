;; a. The lookup in apply-generic for the procedure fails so the 
;; negative clause of the if expression is executed and the coercion
;; procedure for complex->complex is retrieved. This succeeds, based on the
;; introduction to the question (the function is added by Louis).
;; The first cond clause returns true and apply-generic is executed again.

;; This will pass two complex numbers to the exponentiation function again
;; and the process will repeat indefinitely, resulting in an infinite loop.

;; b. If we don't add Louis' procedures then apply-generic will correctly
;; result in an error for the case above, versus running forever. So, he
;; was not correct.

;; c. 
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		; Add type check here and just apply the function
		; if the args are already the same type.
		(if (eq? type1 type2)
		    (apply-generic op a1 a2)
		    (let ((t1->t2 (get-coercion type1 type2))
			  (t2->t1 (get-coercion type2 type1)))
		      (cond (t1->t2
			     (apply-generic op (t1->t2 a1) a2))
			    (t2->t1
			     (apply-generic op a1 (t2->t1 a2)))
			    (apply-generic op a1 a2)
			    (else
			     (error "No method for these types"
				    (list op type-tags)))))))
	      (error "No method for these types"
		     (list op type-tags)))))))


