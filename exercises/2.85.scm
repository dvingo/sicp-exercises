;; the `equ?` function is defined in exercise 2.79.scm

(define (drop arg)
  ;; uses `get-height` procedure from 2.84scm to
  ;; determine when to end recursion, as integer will always
  ;; pass the second if test.
  (if (eq? (get-height arg) 0)
      arg
      (if (equ? (raise (project arg)) arg)
	  (drop (project arg))
	  arg)))

(define (project arg)
  (apply-generic 'project arg))

(put 'project 'scheme-number
     ;; This is the bottom of the chain, so just return.
     (lambda (x) x))
(put 'project 'rational
     ;; Convert rational to integer.
     (lambda (x) (round (/ (numer x) (denom x)))))
(put 'project 'real
     ;; Convert real to integer
     (lambda (x) (round x)))
(put 'project 'complex
     (lambda (x) (real-part x)))

;; Rewrite of `apply-generic` to simplify result.
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
		      ; is not a tower and a coercion cannot be performed
		      ; by simple raising.
		      (if (null? new-args)
			  (error "No method for these types"
				 (list op type-tags))
			  (drop (apply-generic op
					 (car new-args)
					 (cadr new-args))))))))))))
