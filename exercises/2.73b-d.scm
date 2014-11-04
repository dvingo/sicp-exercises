(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	(else
	 ((get 'deriv (operator exp))
	  (operands exp)
	  var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; actual exercise

(define (deriv-sum exp var)
  (make-sum (deriv (addend exp) var)
	    (deriv (augend exp) var)))

(define (deriv-product exp var)
  (make-product (multiplier exp)
		(deriv (multiplicand exp) var))
  (make-product (deriv (multiplier exp) var)
		(multiplicand exp)))

(put 'deriv '+ deriv-sum)
(put 'deriv '* deriv-product)


c.
(define (deriv-exponent exp var)
  (make-product (make-exponentiation
		 (make-product (exponent exp)
			       (base exp))
		 (- (exponent exp) 1))
		(deriv (base exp) var)))

(put 'deriv '** deriv-exponent)

d.
If we switch to ((get (operator exp) 'deriv) (operands exp) var)
we would just have to change the put statements:
(put '+ 'deriv deriv-sum)
(put '* 'deriv deriv-product)
(put '** 'deriv deriv-exponent)
