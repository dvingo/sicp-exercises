; (load "2.56.scm")
(define (augend s)
  (if (= (length (cddr s)) 1)
      (caddr s)
      (make-sum (caddr s) (cadddr s))))
; tests:
;(define s1 '(+ x 1))
;(define s2 (list '+ 'x 'y 'z))
;(augend s1); 1
;(augend s2); (+ y z)

(define (multiplicand p)
  (if (= (length (cddr p)) 1)
      (caddr p)
      (make-product (caddr p) (cadddr p))))

; tests:
;(define s1 '(* x 1))
;(define s2 (list '* 'x 'y 'z))
;(multiplicand s1); 1
;(multiplicand s2); (* y z)

; test 
;(deriv '(* x y (+ x 3)) 'x)
;Value 48: (+ (* x y) (* y (+ x 3)))
