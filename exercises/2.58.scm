;a:

; example input to support:
; (x + (3 * (x + (y + 2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

; (define s3 '(y + 2))
; (sum? s3) ; #t

(define (make-sum a1 a2)
  (list a1 '+ a2))

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
  (list m1 '* m2))

(define (multiplier p)
  (car p))

(define (multiplicand p)
  (caddr p))

; (multiplier m) ; x
; (multiplicand m) ; 4

;(deriv '(x + (3 * (x + (y + 2)))))

;1 ]=> (deriv '(x + (3 * (x + (y + 2)))) 'x)

;Value 59: (0 + ((3 * (0 + (0 + 0))) + (0 * (x + (y + 2)))))

; for now without simplification

; b:
(define (sum? x)
  (cond ((not (pair? x)) #f)
	((null? (cdr x)) (sum? (car x)))
	(else (and (not (product? x))
		   (pair? x) (eq? (cadr x) '+)))))

(define (product? x)
  (cond ((not (pair? x)) #f)
	((and (pair? x) (null? (cdr x))) (product? (car x)))
	((eq? (cadr x) '*) #t)
	((pair? (cddr x)) (product? (cddr x)))))
	
; (define s1 '(x + 3 * (x + y + 2)))
; (sum? s1) ; #f
; (product? s1 ; #t

; (define s2 '((s + y + 2)))
; (sum? s2) ; #t
; (product? s2) ; #f


;; these need to be a bit smarter than this
(define (addend s)
  (car s))
(define (augend s)
  (cddr s))

(define (multiplier p)
  (car p))
(define (multiplicand p)
  (ca
