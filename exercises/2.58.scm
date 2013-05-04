;a:

; example input to support:
; (x + (3 * (x + (y + 2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

; (define s3 '(y + 2))
; (sum? s3) ; #t

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2) (+ a1 a2)))
	(else (list a1 '+ a2))))

(define (addend s)
  (car s))

(define (augend s)
  (caddr s))

; (define s1 (make-sum 'y 3))
; (addend s1); y
; (augend s1); 3

;(define n (make-sum 'y (make-sum 'z 4)))
;(addend n) ; y
;(augend n) (z + 4)


; multiplication:
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

; (define m '(x * 4))
; (product? m); #t

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2) (* m1 m2)))
	(else (list m1 '* m2))))

(define (multiplier p)
  (car p))

(define (multiplicand p)
  (caddr p))

; (multiplier m) ; x
; (multiplicand m) ; 4

;(deriv '(x + (3 * (x + (y + 2)))))

;1 ]=> (deriv '(x + (3 * (x + (y + 2)))) 'x)

;Value 59: (0 + ((3 * (0 + (0 + 0))) + (0 * (x + (y + 2)))))


; ----------------------------------------------------------
; b:
(define (sum? x)
  (cond ((not (pair? x)) #f)
	((null? (cdr x)) (sum? (car x)))
	(else (and (not (product? x))
		   (pair? x) (eq? (cadr x) '+)))))

; assuming multiplication is always done before addition so the following
; would be a product, but not a sum:
; '(a * b) + (c * w))

(define (product? x)
  (cond ((not (pair? x)) #f)
	((and (pair? x) (null? (cdr x))) (product? (car x)))
	((eq? (cadr x) '*) #t)
	((pair? (cddr x)) (product? (cddr x)))))

; tests:	
; (define s1 '(x + 3 * (x + y + 2)))
; (sum? s1) ; #f
; (product? s1 ; #t

; (define s2 '((s + y + 2)))
; (sum? s2) ; #t
; (product? s2) ; #f

(define (addend s)
  (car s))

(define (augend s)
  (caddr s))

(define (multiplier p)
  (define (helper li ret-list)
    (cond ((eq? (car li) '*)
	   ; if we're at the end, return the ret-list
	   (if (null? (cddr li))
	       (if (= (length ret-list) 1)
		   (car ret-list)
		   ret-list)
	       ; otherwise keep going
	       (helper (cdr li) (append ret-list (list (car li))))))
	  (else (helper (cdr li) (append ret-list (list (car li)))))))
  (helper p ()))

; test
; original form:  (* (* x y) (+ x 3))
; (deriv '(x * y * (x + 3)) 'x)
; (deriv '(x * y) 'x); y
; (deriv '(x + 3) 'x); 1

; test:
; (define n1 '(x * y * (x + 3)))
; (multiplier n1) ; (x * y)
; (define n2 '(x * y * z * (x + 3) * 4))
; (multiplier n2) ; (x * y * z * (x + 3))

(define (multiplicand p)
  (define (helper li ret-list)
    (cond ((null? li) ret-list)
	   ((eq? (car li) '*)
	    (if (null? (cdr li))
		ret-list
		(helper (cdr li) (cadr li))))
	   (else (helper (cdr li) ret-list))))
  (helper p ()))

; test:
; (define n1 '(x * y * (x + 3)))
; (multiplicand n1) ; (x + 3)
; (define n2 '(x * y * z * (x + 3) * 4))
; (multiplicand n2) ; 4
