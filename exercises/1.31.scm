; recursive process:
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))
; iterative process:
(define (product-it term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial x)
  (define (id n) n)
  (define (inc a) (+ a 1))
  (product-it id 1 inc x))
(factorial 2)
