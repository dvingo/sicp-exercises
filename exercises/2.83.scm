; The tower is:
;    complex
;      /\
;      |
;     real
;      /\
;      |
;   rational
;      /\
;      | 
;    integer

(define (raise x)
  (cond ((eq (type x) 'real)
	 (raise-real x))
	((eq (type x) 'rational)
	 (raise-rat x))
	((number? x)
	 (raise-scm x))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (raise-scm-num x)
  (make-rat x 1))

(define (raise-rat x)
  (/ (numer x) (denom x)))

(define (raise-real x)
  make-complex-from-real-imag x 0)
  
