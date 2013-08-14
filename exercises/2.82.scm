(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum - TYPE-TAG" datum)))

;; original:
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
	      (let ((coerced-args (coerce-args args)))
		(apply-generic op (car coerced-args) (cdr coerced-args))))))))


; get-coercion is provided in the text
; as the equivalent of the procedure table lookup, but for types.
(define (coerce-args args)
  ;; elements-remaining are the arguments we have left
  ;; to attempt to coerce the other to in case the current
  ;; attempts fail (for arg).
  (define (helper arg rest-of-args return-list elements-remaining)
    (let ((t1->t2 (get-coercion (type-tag arg) (car rest-of-args))))
      (cond ((= (length args) (length return-list))
	       return-list)
	    (t1->t2
	     (helper arg
		     (cdr rest-of-args)
		     (cons (t1->t2 (car rest-of-args)) return-list)
		     elements-remaining))
	    ((null? elements-remaining) 
	     (error "No method for these arguments" args))
	    (else (helper (car rest-of-args)
			  (cons arg (cdr rest-of-args))
			  '()
			  (cdr elements-remaining))))))
  (helper (car args) (cdr args) '((car args)) args))


; This will fail when the elements rotate precisely into the order 
; where the first one fails everytime

; '(2 7.1 1 3+4i)
; '(7.1 1 3+4i 2)
; '(7.1 3+4i 2) - fails
; '(3+4i 2 7.1 1)
; In the general case we will face failure when a type 'lower' in the 
; tower is in between types above it. This occurs because the
; reordering of the elements on failure will still run into
; an attempt of coercing a lower type to a higher type.
; If the elements can be ordered in descending order 
; (higher in the type chain first) then the coercion of
; the remaining arguments to the first's type will succeed.
