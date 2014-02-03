; original
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (>= (length args) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		(let ((t1->t2 (get-coercion type1 type2))
		      (t2->t1 (get-coercion type2 type1)))
		  (cond (t1->t2
			 (apply-generic op (t1->t2 a1) a2))
			(t2->t1
			 (apply-generic op a1 (t2->t1 a2)))
			(else
			 (error "No method for these types"
				(list op type-tags))))))
	      (error "No method for these types"
		     (list op type-tags)))))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (>= (length args) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		(if (eq type1 type2)
		    (apply-generic op a1 a2)
		    (let ((new-args (coerce a1 a2)))
		      ; The current implementation will not return nil
		      ; but this could be used if the hierarchy chain
		      ; is not a tower and a coercion can not be performed
		      ; by simple raising.
		      (if (null? new-args)
			  (error "No method for these types"
				 (list op type-tags))
			  (apply-generic op
					 (car new-args)
					 (cadr new-args)))))))))))
(define (coerce a1 a2)
  (cond ((eq? (type-tag a1) (type-tag a2))
	 (cons a1 a2))
	((higher? a1 a2)
	 (coerce a1 (raise a2)))
	((higher? a2 a1)
	 (coerce (raise a1) a2))))

(define (higher a1 a2)
  (> (get-height a1) (get-height a2)))

(define (get-height x)
  (let ((t (type-tag x))) 
    (cond ((eq? t 'complex)
	   3)
	  ((eq? t 'real)
	   2)
	  ((eq? t 'rational)
	   1)
	  ((eq? t 'integer)
	   0))))
