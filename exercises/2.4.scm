; exercise 2.4

(define (cdr z)
  (z (lambda (p q) q)))

; eg:
(define x 1)
(define y 2)
(cdr (cons x y))
;Value: 2
