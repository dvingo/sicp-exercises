(define (filtered-accumulate combiner null-value term a next b filter)
 (define (iter a result)
    (if (> a b)
	result
	(if (filter a)
	    (iter (next a) (combiner (term a) result))
	    (iter (next a) (combiner null-value result)))))
  (iter a null-value))

; exercise a

;; Define prime? ;; this implementation erroneously returns 1 as #t
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
;; end define prime? ;;

(define (sum term a next b pred)
  (filtered-accumulate + 0 term a next b pred))
(define (inc n) (+ n 1))
(define (cube x) (* x x x))
(define (square x) (* x x))
(define (sum-sq-prime a b)
  (sum square a inc b prime?))
(sum-sq-prime 1 8) ; 88
(+ (square 1) (square 2) (square 3) (square 5) (square 7)) ; = 88

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (ident a) a)
(define (filtered-product term a next b filter)
  (filtered-accumulate * 1 term a next b filter))
(define (prod-pos-int n)
  (define (pos-rel-prime a)
    (if (and (> a 0)
	     (= (gcd a n) 1)
	     (< a n))
	#t
	#f))
  (filtered-product ident 1 inc n pos-rel-prime))

(prod-pos-int 10) ; = 189
(gcd 1 10); 1 *
(gcd 2 10); 2
(gcd 3 10); 1 *
(gcd 4 10); 2
(gcd 5 10); 5
(gcd 6 10); 2
(gcd 7 10); 1 *
(gcd 8 10); 2
(gcd 9 10); 1 *
(* 1 3 7 9) ; = 189
