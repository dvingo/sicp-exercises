(define (average x y)
  (/ (+ x y) 2))
(define (improve guess x)
  (average guess (/ x guess)))

(define (square x)
  (* x x))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (sqrt-iter-2 guess old-guess x)
  (if (< (abs (- guess old-guess)) .0001)
      guess
      (sqrt-iter-2 (improve guess x) guess x)))

(define (sqrt-3 x)
  (sqrt-iter-2 1.0 100.0 x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

(define (sqrt-iter2 guess x)
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (sqrt2 x)
  (sqrt-iter2 1.0 x))

; Exercise 1.8
(define (cube x)
  (* x x x))

(define (good-enough-cube? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (improve-cube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess))
     3.0))

(define (cubert-iter guess x)
  (if (good-enough-cube? guess x)
      guess
      (cubert-iter (improve-cube guess x) x)))

(define (cubert x)
  (cubert-iter 1.0 x))

; Recursive process defined on pg 64
; Exercise 1.9

(define (+ a b)
  (if (= a 0) b (inc (+ (dec a) b))))

(+ 4 5):

(if (= 4 0) 5 (inc (+ (dec 4) 5)))
(inc (+ (dec 4) 5))
(inc (+ 3 5))
(inc (inc (+ (dec 3) 5)))
(inc (inc (inc (+ (dec 2) 5))))
(inc (inc (inc (inc (+ (dec 1) 5)))))
;(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

The process is recursive because these are deferred operations.

(define (+ a b)
  (if (= a 0) b (+ (dec a) (inc b))))

(+ 4 5):
(+ (dec 4) (inc 5))
(+ (dec 3) (inc 6))
(+ (dec 2) (inc 7))
(+ (dec 1) (inc 8))
(+ 0 9)
9
This process is iterative because the state can be summarized by a fixed number of state variables, together with a fixed rule that describes how the state variables should be updated as the process moves from state to state and an (optional) end test that specifies conditions under which the process should terminate.  This is a linear iterative process because the number of steps required grows linearly with n.

; Exercise 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10):
(A 0 (A 1 9))
(* 2 (A 1 9))
;(* 2 (A 0 (A 1 8)))
(* 2 (* 2 (A 1 8)))
;(* 2 (* 2 (A 0 (A 1 7))))
(* 2 (* 2 (* 2 (A 1 7))))
;(* 2 (* 2 (* 2 (A 0 (A 1 6)))))
(* 2 (* 2 (* 2 (* 2 (A 1 6)))))
;(* 2 (* 2 (* 2 (* 2 (A 0 (A 1 5))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (A 1 5))))))
;(* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 4)))))))
;(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 3)))))))
;(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 0 (A 1 2)))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 1 2)))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 1 1))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 2)))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 4))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 8)))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 2)))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 2)))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 2)))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 2)))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 2)))))))))

1024 ; 2^y if x is 1

(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
         2^(2^(2^2)) 2^(2^4) 2^16 (2^4)^2
(A 1 (A 1 (A 0 (A 1 1))))

1024
2048
4096 12
8192 13
16384
32768 15
65536 2^16

(A 3 3)
(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 (A 2 2))
(A 2 (A 1 (A 2 1)))
(A 2 (A 1 2))
(A 2 (A 0 (A 1 1)))
(A 2 (A 0 2))
(A 2 (* 2 2))
(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 (* 2 2)))
(A 1 (A 1 4))
(A 1 (2 ^ 4))
(2 ^ (2 ^ 4))
(2 ^ 16)

f: 2 * n
g: 2 ^ n
h: 2 ^ 2 ^ n

; Exercise 1.11

(define (f n)
  (cond ((< n 3) n)
	(else (+ (f (- n 1))
		 (* 2 (f (- n 2)))
		 (* 3 (f (- n 3)))))))

f(1):  1
f(2):  2
f(3):  2 + 2(1)  + 3(0)
f(4):  4 + 2(2)  + 3(1)
f(5): 11 + 2(4)  + 3(2) : 25
f(6): 25 + 2(11) + 3(4) : 59
f(7): 59 + 2(25) + 3(11) : 142

; (display "a:")
; (display a) (newline)
;  (display "b:")
; (display b) (newline)
;  (display "c:")
; (display c) (newline)
; (display "curr: ") (display curr) (newline) (newline)

(define (f-iter a b c curr final)
  (if (= curr final)
      (+ a (* 2 b) (* 3 c))
      (f-iter (+ a (* 2 b) (* 3 c))
	      a
	      b
	      (+ curr 1)
	      final)))

(define (f n)
  (cond ((< n 3) n)
        ((= n 3) 4)
	(else (f-iter 4 2 1 4 n))))

(f-iter 2 1 0 3 5)
(f-iter 4 2 1 4 5)
(f-iter 11 4 2 5 5)

; exercise 1.12

(define (p row col)
  (cond ((> col row) #f)
        ((or (= row 0) (= col 0) (= row col)) 1)
	(else (+ (p (- row 1) col)
		 (p (- row 1) (- col 1))))))

; exercise 1.15a

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(sine 12.15)
(p (sine 4.05))
(p (p (sine 1.35)))
(p (p (p (sine 0.45))))
(p (p (p (sine 0.45))))
(p (p (p (p (sine 0.15)))))
(p (p (p (p (p (sine 0.05)))))) = 5

; b
space is linear
steps? exponential

; exercise 1.16
; linear recursive (logarithmic steps):
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(fast-expt 2 8)
(square (fast-expt 2 4))
(square (square (fast-expt 2 2)))
(square (square (square (fast-expt 2 1))))
(square (square (square (* 2 (fast-expt 2 0)))))
(square (square (square (* 2 1))))
(square (square (square 2)))
(square (square 4))
(square 16)
256

; iterative algorithm:
(define (even? n)
  (= (remainder n 2) 0))
(define (square n) (* n n))
(define (exp b n)
  (it-exp b n 1))
(define (it-exp b n a)
  (cond ((= n 2) (* a (square b)))
	((even? n) (it-exp b (/ n 2) (square b)))
	(else (* b (it-exp b (- n 1) a)))))

; exercise 1.17
; linear algorithm:
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))
(define (double n) (+ n n))
(define (halve n) (/ n 2))
; logarithmic algorithm
(define (fast* a b)
  (cond ((= b 1) (+ a 1))
	((= b 2) (double a))
	((even? b) (+ (fast* a (halve b)) (fast* a (halve b))))
	(else (+ a (fast* a (- b 1))))))

(fast* 3 3)
(+ 3 (fast* 3 2))
(+ 3 (double 3))
(+ 3 6)
9

(fast* 2 5)
(+ 2 (fast* 2 4))
(+ 2 (+ (fast* 2 2) (fast* 2 2)))
(+ 2 (+ (double 2) (double 2)))
(+ 2 (+ 4 4))
(+ 2 8)
10

; exercise 1.18
; iterative process of multiplication
(define (iter* a b c)
  (cond ((= b 1) c)
        ((even? b) (iter* a (halve b) (+ (double a) c)))
	(else (iter* a (- b 1) (+ a c)))))
(define (it* a b)
  (iter* a b 0))

(it* 3 4)
(iter* 3 4 0)
(iter* 3 2 (+ 6 0))
(iter* 3 1 (+ 6 6))
12


(it* 2 5)
(iter* 2 5 0)
(iter* 2 4 2)
(iter* 2 2 (+ 4 2))
(iter* 2 1 (+ 4 6))
10

; exercise 1.20
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd 40 6)
(gcd 6 (remainder 40 6))
(gcd 6 4)
(gcd 4 (remainder 6 4))
(gcd 4 2)
(gcd 2 (remainder 4 2))
(gcd 2 0)
0

(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
; and on and on...

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
	(else (remainder (* base (expmod base (- exp 1) m)) m))))

; exercise 1.21

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

(smallest-divisor 199): 199
(smallest-divisor 1999): 1999
(smallest-divisor 19999): 7

; exercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (cond ((>= start end) #t)
	((timed-prime-test start)
	 (search-for-primes (+ start 2) end))))

; exercise 1.23
; for 1.24 and 1.25 need to get a working timed-prime-test

; exercise 1.26
The change is that 2 calls are made to expmod instead of just one which
gets rid of the logarithmic performance.

; exercise 1.27
; 561, 1105, 1729, 2465, 2821, 6601

(define (test-charmichael n)
  (define (try a)
    (cond ((= a n) #t)
	  ((= (expmod a n n) a) (try (+ a 1)))
	  (else #f)))
  (try 1))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
	(else (remainder (* base (expmod base (- exp 1) m)) m))))

; exercise 1.28

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((or (= base 1) (= base (- m 1))) 0)
	((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
	(else (remainder (* base (expmod base (- exp 1) m)) m))))

; no idea...

(define (mr-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) a))
  (try-it (+ 1 (random (- n 1)))))

; in general lambda is used to create procedures in the same way as define, except that no name is specified for the procedure.
(lambda (<formal parameters>) <body>)

like:

(lambda (x) (+ x 1))
(define (plus4 x) (+ x 4)) == (define plus4 (lambda (x) (+ 4 x)))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((positive? test-value)
		 (search f neg-point midpoint))
		((negative? test-value)
		 (search f midpoint pos-point))
		(else midpoint))))))

(define (close-enough? x y) (< (abs (- x y)) 0.001))
(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b))
	  ((and (negative? b-value) (positive? a-value))
	   (search f b a))
	  (else
	   (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)
(define (average x y) (/ (+ x y) 2))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))
; ^^ this technique is called average damping

; from other file:
(define (cube a) (* a a a))
(define (sum-cubes a b)
  (if (> a b) 0 (+ (cube a) (sum-cubes (+ a 1) b))))

(sum-cubes 1 2)
(+ (cube 1) (sum-cubes (+ 1 1) 2))
(+ 1 (sum-cubes 2 2))
(+ 1 (+ (cube 2) (sum-cubes (+ 2 1) 2)))
(+ 1 (+ 8 0))
9

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (identity x) x)

(define (sum-ints a b)
  (sum identity a inc b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(integral cube 0 1 0.01)
(* (sum cube (+ 0 (/ 0.01 2.0)) add-dx 1) 0.01)
(* (+ (cube 0) (sum cube (add-dx 0) add-dx 1)) 0.01)
(* (+ (cube 0) (sum cube (+ 0 0.01) add-dx 1)) 0.01)
(* (+ (cube 0) (sum cube 0.01 add-dx 1)) 0.01)
(* (+ (cube 0) (+ (cube 0.01) (sum cube (add-dx 0.01) add-dx 1))) 0.01)
(* (+ 0 (+ 1.0e-6 (sum cube (add-dx 0.01) add-dx 1))) 0.01)
(* (+ 0 (+ 1.0e-6 (sum cube 0.02 add-dx 1))) 0.01)
(* (+ 0 (+ 1.0e-6 (+ 8.0e-6 (sum cube (add-dx 0.02) add-dx 1)))) 0.01)


(integral cube 0 1 0.01)
.24998750000000042
(integral cube 0 1 0.001)
.249999875000001
(The exact value of the integral of cube between 0 and 1 is 1/4.)

h = (b - a) / n ; for some even integer n
ysub k = f (a + kh)

(define (simpson f a b n)
  (let (h (/ (- b a) n))
    (define (next-a k)
      (* (+ 1 (remainder k 2)) (+ a (* k h))))
    (* (/ h 3) (sum f a next-a b))))

; again!
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (simpsons f a b n)
  (define h (/ (- b a) n))
  (define (inc x) (+ x 1))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (* (cond ((odd? k) 4)
	     ((or (= k 0) (= k n)) 1)
	     ((even? k) 2))
       (y k)))
  (/ (* h (sum term 0 inc n)) 3))
(define (cube x) (* x x x))
(simpsons cube 0 1 4)
(/ (* (/ (- 1 0) 4) (sum term 0 inc 3)) 3)
(/ (* .25 (sum term 0 inc 4)) 3)
(/ (* .25 (+ (term 0) (sum term (inc 0) inc 4))) 3)
(/ (* .25 (+ (* 1 (y 0)) (sum term (inc 0) inc 4))) 3)
(/ (* .25 (+ (* 1 (f (+ 0 (* 0 .25))))) (sum term (inc 0) inc 4))) 3)

; exercise 1.30

; generalized form of summation
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))

(define (inc n) (+ n 1))
(define (cube x) (* x x x))
(define (sum-cubes a b)
  (sum cube a inc b))
(sum-cubes 1 10)
;3025

; exercise 1.31
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

; exercise 1.32

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

; exercise 1.33
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

; exercise 1.34
f will be bound to the formal parameter g and then will attempt
to have 2 applied to it.  As 2 is not a function, you cannot apply
2 to 2, resulting in a runtime error.

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((positive? test-value)
		 (search f neg-point midpoint))
		((negative? test-value)
		 (search f midpoint pos-point))
		(else midpoint))))))
(define (average x y) (/ (+ x y) 2))
(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
	   (search f a b))
	  ((and (negative? b-value) (positive? a-value))
	   (search f b a))
	  (else
	   (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)


; finding the fixed point of cos
scheme@(guile-user)> (cos 1)
$6 = 0.54030230586814
scheme@(guile-user)> (cos 0.54030230586814)
$7 = 0.857553215846393
scheme@(guile-user)> (cos 0.857553215846393)
$8 = 0.654289790497779
scheme@(guile-user)> (cos 0.654289790497779)
$9 = 0.793480358742566
scheme@(guile-user)> (cos 0.793480358742566)
$10 = 0.701368773622756
scheme@(guile-user)> (cos 0.701368773622756)
$11 = 0.763959682900655
scheme@(guile-user)> (cos 0.763959682900655)
$12 = 0.722102425026707
scheme@(guile-user)> (cos 0.722102425026707)
$13 = 0.750417761763761
scheme@(guile-user)> (cos 0.750417761763761)
$14 = 0.73140404242251
scheme@(guile-user)> (cos 0.73140404242251)
$15 = 0.744237354900557
scheme@(guile-user)> (cos 0.744237354900557)
$16 = 0.735604740436347
scheme@(guile-user)> (cos 0.735604740436347)
$17 = 0.741425086610109
scheme@(guile-user)> (cos 0.741425086610109)
$18 = 0.737506890513243
scheme@(guile-user)> (cos 0.737506890513243)
$19 = 0.740147335567876
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (/ x y)) 1.0))

;;;; - from other file ^^^^^^^^

; exercise 1.35
(define (gold x)
  (fixed-point (lambda (y) (+ 1 (/ 1 y))) 1.0))

; exercise 1.36
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display "guess: ")
    (display guess) (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define x-x
  (fixed-point (lambda (y) (/ (log 1000) (log y))) 2.0))
; console output
(define x-x
  (fixed-point (lambda (y) (/ (log 1000) (log y))) 2.0))
guess: 2.
guess: 9.965784284662087
guess: 3.004472209841214
guess: 6.279195757507157
guess: 3.759850702401539
guess: 5.215843784925895
guess: 4.182207192401397
guess: 4.8277650983445906
guess: 4.387593384662677
guess: 4.671250085763899
guess: 4.481403616895052
guess: 4.6053657460929
guess: 4.5230849678718865
guess: 4.577114682047341
guess: 4.541382480151454
guess: 4.564903245230833
guess: 4.549372679303342
guess: 4.559606491913287
guess: 4.552853875788271
guess: 4.557305529748263
guess: 4.554369064436181
guess: 4.556305311532999
guess: 4.555028263573554
guess: 4.555870396702851
guess: 4.555315001192079
guess: 4.5556812635433275
guess: 4.555439715736846
guess: 4.555599009998291
guess: 4.555493957531389
guess: 4.555563237292884
guess: 4.555517548417651
guess: 4.555547679306398
guess: 4.555527808516254
guess: 4.555540912917957

; using average damping
(define x-x
  (fixed-point (lambda (y) (/ (+ y (/ (log 1000) (log y))) 2)) 2.0))
; console output:
1 ]=> (define x-x
  (fixed-point (lambda (y) (/ (+ y (/ (log 1000) (log y))) 2)) 2.0))
guess: 2.
guess: 5.9828921423310435
guess: 4.922168721308343
guess: 4.628224318195455
guess: 4.568346513136242
guess: 4.5577305909237005
guess: 4.555909809045131
guess: 4.555599411610624
guess: 4.5555465521473675

; exercise 1.37
(define (cont-frac n d k)
  (define (iter i holder)
    (if (= i k)
	holder
	(iter (+ i 1) (/ (n (- k i)) (+ (d (- k i)) holder)))))
  (iter 1 (/ (n k) (d k))))

(cont-frac (lambda (i) 1.0)
	   (lambda (i) 1.0)
	   10)

; k of 10 was too small, 100 was sufficient
; b (recursive process)
(define (cont-frac-rec n d k)
  (define (helper i)
    (cond ((= i k) (d i))
	  (else (+ (d (- i 1)) (/ (n i) (helper (+ i 1)))))))
  (/ (n 1) (helper 2)))

(cont-frac-rec (lambda (i) 1.0)
	       (lambda (i) 1.0)
	       100)
(/ 1.0 (helper 2))
(/ 1.0 (+ 1.0 (/ 1.0 (helper 3))))
...
(helper k) == (+ 1.0 (/ 1.0 1.0))

; exercise 1.38

(define (d i)
  (cond ((not (= (remainder (+ i 1) 3) 0)) 1)
	(else (* 2 (/ (+ i 1) 3)))))
(cont-frac (lambda (i) 1.0) d 100)
(cont-frac-rec (lambda (i) 1.0) d 100)

; e = 2.71828182846
; e - 2 = .71828182846

; exercise 1.39
(define (cont-frac-sub n d k)
  (define (iter i holder)
    (if (= i k)
	holder
	(iter (+ i 1) (/ (n (- k i)) (- (d (- k i)) holder)))))
  (iter 1 (/ (n k) (d k))))

(define (odd? i) (= (remainder i 2) 1))

(define (tan-cf x k)
  (cont-frac-sub (lambda (i)
		   (if (= i 1) x (* x x)))
	       (lambda (i) (+ i (- i 1)))
	       k))
(tan-cf 3.1415926535 1000) ; == 0

; pi / 6 = 0.52359877
(tan-cf 0.52359877 1000) ; tan(pi/6) == 0.5773502 == sqrt(3)/3

; page 110 avg damping
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10) ; 55

; exercise 1.40
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* (square x) a) (* x b) c)))

; to be used for example like this:
(define (cubic-root a b c)
  (newtons-method (cubic a b c) 1))

; exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))

(define (inc x)
  (+ x 1))

(define (add-two x)
  ((double inc) x))

(((double (double double)) inc) 5) ; 21

(((double double) inc) 1) ; 5
(((double double) inc) 2) ; 6
(((double double) inc) 3) ; 7
(((double double) inc) 4) ; 8
(((double double) inc) 5) ; 9

(((double (double double)) inc) 1) ; 17
(((double (double double)) inc) 2) ; 18 
(((double (double double)) inc) 3) ; 19
(((double (double double)) inc) 4) ; 20
(((double (double double)) inc) 5) ; 21
(((double (double double)) inc) 6) ; 22

; exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6) ; 49

; exercise 1.43
(define (repeated f n)
  (define (helper g a)
    (if (= a 1)
        g
	(compose g (helper g (- a 1)))))
  (lambda (x)
    ((helper f n) x)))

(repeated f 3)
(compose g (helper g 2))
(compose g (compose g (helper g 1)))
(compose g (compose g g))

; exercise 1.44
(define (smooth f)
  (define dx 0.00001)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (g x)
  (+ x 2.0))

 (/ (+ (g (- 3.1 dx)) (g 3.1) (g (+ 3.1 dx))) 3.0)))

((repeated (smooth g) 2) 3.1)
(define (n-smooth f n)
    (repeated (smooth f) n))

((n-smooth sin 2) (/ pi 2))
((n-smooth sin 3) (/ pi 2))
((n-smooth sin 4) (/ pi 2))
((n-smooth sin 5) (/ pi 2))
((n-smooth sin 6) (/ pi 2))
((n-smooth sin 10) (/ pi 2))

; exercise 1.45

(define (average x y) (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))
(define tolerance 0.0000001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display "guess: ")
    (display guess) (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

; does not converge:
(define (sqrt x)
      (fixed-point (lambda (y) (/ x y)) 1.0))

; does converge with one level of avg damping:
(define (sqrt x)
      (fixed-point (average-damp (lambda (y) (/ x y)))
1.0))
(define (cbrt x)
      (fixed-point (average-damp (lambda (y) (/ x (expt y 2))))
1.0))
; depends upon repeated and compose
(define (four-rt x)
    (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (expt y 3))))
1.0))

(define (five-rt x)
    (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (expt y 4))))
1.0))
  

(define (nth-rt n x num-avg)
    (fixed-point ((repeated average-damp num-avg) (lambda (y) (/ x (expt y (- n 1)))))
1.0))

; fourth root requires 2 average damps
; fifth root requires 2 average damps
; sixth root converges at 2
; seventh root converges at 2
; 8th root requires 3
; 9th root requires 3
; 10th root requires 3
; 11,12,13 3
; 16 requires 4

; number of average damps needed is:
; (floor (sqrt n))

(define (nth-rt n x)
    (fixed-point ((repeated average-damp(floor (sqrt n)))
		  (lambda (y) (/ x (expt y (- n 1)))))
1.0))

; exercise 1.46

(define (iterative-improve good-enough? improve-guess)
  (define (helper guess x)
    (if (good-enough? guess x)
	guess
	(helper (improve-guess guess x) x)))
  helper)

 (define (average x y)
      (/ (+ x y) 2))

(define (improve guess x)
      (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

; original:
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

(define (new-sqrt guess x)
  ((iterative-improve good-enough? improve) guess x))
  
; Chapter 2

; cons stands for construct.  The names car and cdr derive from the original 
; implementation of Lisp on the IBM 704.  That machine had an addressing scheme
; that allowed one to reference the "address" and "decrement" parts of a
; memory loction.  Car stands for "Contents of Address part of Register"
; and cdr (pronounced "could-er") stands for "Contents of Decrement part of
; Register."

; exercise 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((and (negative? n) (negative? d))
	   (cons (/ (* -1 n) g) (/ (* -1 d) g)))
	  ((and (negative? n) (positive? d))
	   (cons (/ n g) (/ d g)))
	  ((and (positive? n) (negative? d))
	   (cons (/ (* -1 n) g) (/ (* -1 d) g)))
	  (else (cons (/ n g) (/ d g))))))

; exercise 2.2
(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment line-segment)
  (let ((x1 (x-point (start-segment line-segment)))
	(x2 (x-point (end-segment line-segment)))
	(y1 (y-point (start-segment line-segment)))
	(y2 (y-point (end-segment line-segment))))
	(make-point 
	 (/ (+ x1 x2) 2)
	 (/ (+ y1 y2) 2))))

(define (print-point p)
         (newline)
         (display "(")
         (display (x-point p))
         (display ",")
         (display (y-point p))
         (display ")"))

(define p1 (make-point 4 4))
(define p2 (make-point 8 8))

(define seg (make-segment (make-point 4 4) (make-point 8 8)))

(print-point (make-point (/ (+ 4 8) 2) (/ (+ 4 8) 2)))

; exercise 2.3

; takes two points top-left and bottom-right
(define (make-rec top-left bottom-right)
  (cons (cons top-left 
	(make-point (x-point bottom-right) (y-point top-left)))
	(cons (make-point (x-point top-left) (y-point bottom-right))
	      (make-point (x-point bottom-right) (y-point bottom-right)))))

(make-rec (make-point 2 4) (make-point 4 0))

; four points of the rectangle
(define (tl rec)
  (car (car rec)))
(define (tr rec)
  (cdr (car rec)))
(define (bl rec)
  (car (cdr rec)))
(define (br rec)
  (cdr (cdr rec)))

; version 2:
(define (make-rec top-segment left-segment)
  (cons top-segment left-segment))

(define top-segg (make-segment (make-point 2 4) (make-point 4 4)))
(define left-segg (make-segment (make-point 2 4) (make-point 2 0)))

(define my-rec (make-rec top-segg left-segg))

(define (tl rec)
  (start-segment (car rec)))
(define (tr rec)
  (end-segment (car rec)))
(define (bl rec)
  (end-segment (cdr rec)))
(define (br rec)
  (make-segment (x-point (end-segment (car rec)))
		(y-point (end-segment (cdr rec)))))
  
(define (top-seg rec)
  (make-segment (tl rec) (tr rec)))
(define (bot-seg rec)
  (make-segment (bl rec) (br rec)))
(define (left-seg rec)
  (make-segment (tl rec) (bl rec)))
(define (right-seg rec)
  (make-segment (tr rec) (br rec)))
(define (square x) (* x x))
(define (length segment)
  (let ((x1 (x-point (start-segment segment)))
	(x2 (x-point (end-segment segment)))
	(y1 (y-point (start-segment segment)))
	(y2 (y-point (end-segment segment))))
    (sqrt (+ (square (- x2 x1)) (square (- y2 y1))))))

(define (rect-perimeter rec)
  (+ (length (top-seg rec))
     (length (bot-seg rec))
     (length (left-seg rec))
     (length (right-seg rec))))

(define (rect-area rec)
  (* (length (top-seg rec))
     (length (left-seg rec))))

; exercise 2.4

(define (cdr z)
  (z (lambda (p q) q)))

; eg:
(define x 1)
(define y 2)
(cdr (cons x y))
;Value: 2

; exercise 2.5
(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (define (helper a num-divisions)
    (cond ((not (= (remainder a 2) 0)) num-divisions)
	  (else (helper (/ a 2) (+ 1 num-divisions)))))
  (helper z 0))

(define (cdr z)
  (define (helper a num-divisions)
    (cond ((not (= (remainder a 3) 0)) num-divisions)
	  (else (helper (/ a 3) (+ 1 num-divisions)))))
  (helper z 0))

;exercise 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; derive one:
(add-1 zero)
(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
(lambda (f) (lambda (x) (f x)))

; derive two:
(add-1 one)
(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

; a and b are function of one argument
(define (church-add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
; apply f b times then apply that function a times.
