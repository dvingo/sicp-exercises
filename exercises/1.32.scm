(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner
       (term a)
       (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))
(define (inc n) (+ n 1))
(define (cube x) (* x x x))
(define (sum-cubes a b)
  (sum cube a inc b))
(sum-cubes 1 10)

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (factorial x)
  (define (id n) n)
  (define (inc a) (+ a 1))
  (product id 1 inc x))
(factorial 2)

; b
(define (accumulate-it combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (product term a next b)
  (accumulate-it * 1 term a next b))

(define (factorial x)
  (define (id n) n)
  (define (inc a) (+ a 1))
  (product id 1 inc x))
(factorial 2)
