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
; exercise 1.23
; for 1.24 need a working runtime function


; exercise 1.26

; exercise 1.27

; exercise 1.28
; non-trivial expmod
(define (expmod-nt base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (if (and (not (= base 1))
		  (not (= base (- m 1)))
		  (= (remainder (square base) m) 1))
		  0
		  (remainder (square (expmod base (/ exp 2) m)) m)))
	(else (remainder (* base (expmod base (- exp 1) m)) m))))
; doesn't seem to be working 561 should return #f
(define (miller-rabin-test n)
  (define (try-it a)
    (if (= (expmod-nt a n n) 0)
	#f
	(= (expmod-nt a n n) a)))
  (try-it (+ 1 (random (- n 1)))))

; from the text:
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))



; next section notes:
; in general lambda is used to create procedures in the same way as define, except that no name is specified for the procedure.
(lambda (<formal parameters>) <body>)

;like:

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

; exercise 2.7
; from the text:
(define (make-interval a b) (cons a b))

(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))s

; exercise 2.8
; the minimum value the difference can be is the difference
; of the lower bound of a and the upper bound of b
; the maximum value the difference can be is the differnce
; of the upper bound of a with the lower bound of b
; (4 8) (1 3)
; assumes a is the larger resistance
(define (sub-interval a b)
  (make-interval (- (lower-bound a) (upper-bound b))
		 (- (upper-bount a) (lower-bound b))))

; from the text:
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

; exercise 2.9
(define (width interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))

(define x (make-interval 1.3 3.4))
(define y (make-interval 2.3 5.8))

(width x) ; 1.0499
(width y) ; 1.75
(add-interval x y) ; (3.5999999999999996 . 9.2)
(width (add-interval x y)) ; 2.8
(width (mul-interval x y)) ; 8.365

; exercise 2.10
(define (div-interval x y)
  (if (or (= (upper-bound y) 0)
	  (= (lower-bound y) 0)
	  (and (> (upper-bound y) 0)
	       (< (lower-bound y) 0)))
      (error "attempted division by zero!")
      (mul-interval x
		    (make-interval (/ 1.0 (upper-bound y))
				   (/ 1.0 (lower-bound y))))))

(div-interval (make-interval 0 2) x) ; works
(div-interval x (make-interval -1.2 2)) ; fail!
(div-interval x (make-interval 0 2)) ; fail!
(div-interval x (make-interval 0.1 0)) ; fail!

; exercise 2.11
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define (print-mm x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (display "p1-p4: ")(display ", ")(display p1)
    (display ", ")(display p2)(display ", ")(display  p3)(display ", ") (display p4)))

; (print-mm x y)
; p1-p4: , 2.9899999999999998, 7.54, 7.819999999999999, 19.72

(define a (make-interval -1.3 3.3))
(define b (make-interval  3.2 3.4))
(define (positive? x) (> x 0))
(define (negative? y) (< y 0))
(define (mul-interval-2 x y)
  (let ((ub-x (upper-bound x))
	(lb-x (lower-bound x))
	(ub-y (upper-bound y))
	(lb-y (lower-bound y)))
  (cond ((and (positive? ub-x)
	      (positive? lb-x)
	      (positive? ub-y)
	      (positive? lb-y))
	 (make-interval (* lb-x lb-y) (* ub-x ub-y)))
	((and (positive? ub-x)
	      (positive? lb-x)
	      (positive? ub-y)
	      (negative? lb-y))
	 (make-interval (* lb-x lb-y) (* ub-x ub-y)))
	((and (positive? ub-x)
	      (positive? lb-x)
	      (negative? ub-y)
	      (negative? lb-y))
	 (make-interval (* ub-x lb-y) (* lb-x ub-y)))
	((and (positive? ub-x)
	      (negative? lb-x)
	      (positive? ub-y)
	      (positive? lb-y))
	 (make-interval (* lb-x ub-y) (* ub-x ub-y)))
	((and (positive? ub-x)
	      (negative? lb-x)
	      (positive? ub-y)
	      (negative? lb-y))
	 (make-interval (max (* lb-x ub-y) (* ub-x lb-y))
			(max (* ub-x ub-y) (* lb-x lb-y))))
	((and (positive? ub-x)
	      (negative? lb-x)
	      (negative? ub-y)
	      (negative? lb-y))
	 (make-interval (* ub-x lb-y) (* lb-x lb-y)))
	((and (negative? ub-x)
	      (negative? lb-x)
	      (positive? ub-y)
	      (positive? lb-y))
	 (make-interval (* lb-x ub-y) (* ub-x lb-y)))
	((and (negative? ub-x)
	      (negative? lb-x)
	      (positive? ub-y)
	      (negative? lb-y))
	 (make-interval (* lb-x ub-y) (* lb-x lb-y)))
	((and (negative? ub-x)
	      (negative? lb-x)
	      (negative? ub-y)
	      (negative? lb-y))
	 (make-interval (* ub-x ub-y) (* lb-x lb-y))))))
	

(mul-interval a b); (-4.42 . 11.2199999999999)
(mul-interval-2 a b); (-4.42 . 11.2199999999999)

; exercise 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent center tol)
  (make-interval (- center tol) (+ center tol)))

(define (percent interval)
  (/ (width interval) (center interval)))

(define x (make-interval 1.3 3.4))
(define y (make-interval 2.3 5.8))

(center x); 2.35
(width x); 1.0499999999999998
(make-center-width 2.35 1.0499999999999998); (1.3000000000000003 . 3.4)
(percent x) ;.4468085106382978

;  (/ (width x) (center x))
;Value: .4468085106382978

; (* .4468 2.35): 1.04998

; (+ 1.04998 2.35) : 3.3999800000000002

; (- 2.35 1.04998) : 1.3000200000000002

; exercise 2.13

(define a (make-center-percent 3 .2))
(define b (make-center-percent 2 .3))
(percent (mul-interval a b)); = .2145214521452145
(define a (make-center-percent 3 .2))
(define b (make-center-percent 2 .5)); percent: .31147540932606564

; this looks like the approximate percentage tolerance of the product
; of two intervals is the average of the percentage tolerances of the factors.
(/ (+ .2 .3) 2); = .25
(/ (+ .2 .5) 2); = .35


; exercise 2.14

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))

(define a (make-center-percent 3 .00000001))
(define b (make-center-percent 2 .00000001))
(width a)
(width b)
(define c (div-interval a b))
(define d (div-interval a b))
(make-center-percent (center c) (percent c))
; (1.4999999916666666 . 1.5000000083333334)
(make-center-percent (center d) (percent d))
; (1.4999999916666666 . 1.5000000083333334)

; exercise 2.15
She is right because each use of an uncertain number brings a certain amount
of error into the result.  This is due to the nature of intervals, in that
we are not dealing with exact values. Instead they have tolerances of error
that we must take into account when performing arithmetic on them. The more
operations on the unknown quantities, the higher our tolerance must be.

; exercise 2.16
Similar to the answer to exercise 2.15, this is due to the nature of intervals and
that we must accept some tolerance for error. It is to devise an interval-arithmetic
package that does not have this shortcoming, as this "feature" is inherent when
dealing with intervals.

; exercise 2.17
(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

(last-pair (list 23 72 149 34)) ; (34)

; exercise 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(else
	 (+ (cc amount
		(except-first-denomination
		 coin-values))
	    (cc (- amount
		   (first-denomination coin-values))
		coin-values)))))
(define (first-denomination values)
  (car values))
(define (except-first-denomination values)
  (cdr values))
(define (no-more? values)
  (null? values))

; no the order does not matter as the nature of the algorithm
; is to break the problem into two subproblems: one where the first
; coin in the input list is considered, and one where it is discarded.
; So as long as all coins are still considered, the result will be the
; same.

; exercise 2.20
(define (same-parity a . more)
  (define (filter f the-list ans-list)
    (cond ((null? the-list) ans-list)
	  ((f (car the-list))
	   (append (list (car the-list)) (filter f (cdr the-list) ans-list)))
	  (else (filter f (cdr the-list) ans-list))))
  (if (even? a)
      (append (list a) (filter even? more ()))
      (append (list a) (filter odd? more ()))))
; alternative form:
(define (same-parity a . more)
  (define (filter f the-list)
    (cond ((null? the-list) ())
	  ((f (car the-list))
	   (cons (car the-list) (filter f (cdr the-list))))
	  (else (filter f (cdr the-list)))))
  (if (even? a)
      (cons a (filter even? more))
      (cons a (filter odd? more))))
;(same-parity 2 3 4 5 6 7)
;(same-parity 1 2 3 4 5 6 7)

; exercise 2.21
(define (map proc items)
  (if (null? items)
      ()
      (cons (proc (car items))
	    (map proc (cdr items)))))

(define (square x) (* x x))
(define (square-list items)
  (if (null? items)
      ()
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list2 items) (map square items))

; exercise 2.22
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons (square (car things))
		    answer))))
  (iter items ()))

; It is in reverse because the cons operator's first argument is
; the first element of the passed list, while the second is
; the existing answer list.
; Thus the answer list is built back to front, with the first
; item of the passed 'items' list being added to the list first
; (in last place).
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons answer
		    (square (car things))))))
  (iter items ()))

; The problem here is that you end up cons'ing lists of lists
; instead of lists of individual elements due to the first
; item in the cons being the empty list instead of it being the last

; exercise 2.23
(define (for-each f items)
  (define (helper i)
    (f i)
    (for-each f (cdr items)))
  (if (null? items)
      #t
      (helper (car items))))

(for-each (lambda (x) (newline) (display x)) (list 57 321 88))
;57
;321
;88
;Value: #t

; exercise 2.24
1 ]=> (list 1 (list 2 (list 3 4)))

; (1 (2 (3 4)))

; ascii art of the box and pointer
; the numbers are pointers to numbers and the empty 
; brackets contain dots with the subsequent arrows coming
; out of them.
[1][]->[2][]->[3][]->[4][/]

; ascii art of the tree represented by the list
(1 (2 (3 4)))
      /\
     /  \
    1   (2 (3 4))
         /\
        /  \
       2   (3 4)
            /\
           /  \
          3    4

; exercise 2.25
(define y (list 1 3 (list 5 7) 9))
(car (cdr (caddr y)))

(define y (list (list 7)))
(caar y)

(define y (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(cadr (cadr (cadr (cadr (cadr (cadr y))))))

; exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y); (1 2 3 4 5 6)
(list x y); ((1 2 3) (4 5 6))

; exercise 2.27
(define (reverse l)
  (if (null? (cdr l))
      (car l)
      (cons (reverse (cdr l)) (list (car l)))))

(define (deep-reverse l)
  (if (not (list? l))
      l
      (if (null? (cdr l))
	  (deep-reverse (car l))
	  (cons (deep-reverse (cdr l)) (list (deep-reverse (car l)))))))

(define x (list (list 1 2) (list 3 4)))
(reverse x)
; ((3 4) (1 2))
(deep-reverse x)
; ((4 3) (2 1))

; exercise 2.28
(define (fringe l)
  (define (helper li ans)
    (cond ((not (list? li)) (append ans (list li)))
	  ((null? (cdr li)) (helper (car li) ans))
	  (else
	   (append (helper (car li) ans) (helper (cdr li) ans)))))
(helper l ()))

; exercise 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define a (make-branch 5 5))
(define b (make-branch 6 6))
(define mobi (make-mobile a b))
; a
(define (left-branch m) (car m))
(define (right-branch m) (cadr m))
(define br (make-branch 4 5))
(define (branch-length b) (car b))
(define (branch-structure b) (cadr b))

(define c (make-branch 10 4))
(define d (make-branch 10 4))
(define m1 (make-mobile c d))

(define e (make-branch 4 m1))
(define f (make-branch 5 7))
(define m2 (make-mobile e f))
; w(f) = 7
; w(e) = w(m1) = 4 + 3
; b
(define (total-weight mobile)
  (define (weight-helper b)
    (cond ((list? (branch-structure b))
	   (+ (weight-helper (left-branch (branch-structure b)))
	      (weight-helper (right-branch (branch-structure b)))))
	  (else (branch-structure b))))
  (+ (weight-helper (left-branch mobile))
     (weight-helper (right-branch mobile))))

(total-weight m2); 14

; c
(define (branch-weight branch)
  (if (list? (branch-structure branch))
      (total-weight (branch-structure branch))
      (branch-structure branch)))
(define (torque branch)
    (* (branch-length branch)
       (branch-weight branch)))
(define (balanced? mobile)
  (define (branch-balanced? branch)
    (if (list? (branch-structure branch))
	(balanced? (branch-structure branch))
	#t))
  (and (= (torque (left-branch mobile))
	  (torque (right-branch mobile)))
       (branch-balanced? (left-branch mobile))
       (branch-balanced? (left-branch mobile))))

; d
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

; You should only need to redefine the selectors from exercise a,
; however in my code I relied the implementation of the
; underlying data structure of the branches and mobiles.
; I used the predicate list? instead of hiding this detail in
; another predicate such as mobile?. So I would also need
; to replace all instances of list? with pair?.
(define (left-branch m) (car m))
(define (right-branch m) (cdr m))

(define (branch-length b) (car b))
(define (branch-structure b) (cdr b))

; exercise 2.30
(define (square-tree tree)
  (cond ((null? tree) ())
	((not (pair? tree)) (* tree tree))
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (* sub-tree sub-tree)))
       tree))
	 
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

; exercise 2.31
(define (tree-map fn tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (fn sub-tree)))
       tree))
(define (square-tree tree) (tree-map square tree))

; exercise 2.32
(define (subsets s)
  (if (null? s)
      (list ())
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x)
			    (cons (car s) x)) rest)))))

(subsets (list 1 2 3))
(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

rest will be a list of lists, so when you cons the car of the current s
with each of rest's members the result is a list of lists with the first element
of the passed list (s) added to it. The reason you don't end up with any lists containing
a distinct element and nil (for example (2 ()) ) is due to the base case returning a
list containing nil, which gets decomposed by map (which returns a list) and
then getting appended to the recursive result of the (subsets) of the cdr of the current set.

Or explained via the example given:
in the example given the recursive subset calls result in ((2 3) (2) (3) ()) so when cons'ing with
1 we get ((1 2 3) (1 2) (1 3) (1)) - which gets appended to the result
this repeats for (2 3) where we get ((3) ())
when cons'ed with 2 we get ((2 3) (2)) - which gets appended to the result
this repeats for (3) where we get ()
when cons'ed with 3 we get ((3) ()) - which gets appended to the result

; exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(if (pair? higher-terms)
		    (+ this-coeff (* x (horner-eval x higher-terms)))
		    (+ this-coeff (* x higher-terms))))
	      0
	      coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1)); 79

; exercise 2.35
; from the text:
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
; from the text:
(define (enumerate-tree tree)
  (cond ((null? tree) ())
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))
; (1 2 3 4 5)

(define (count-leaves t)
  (accumulate + 0 (map (lambda (r) 1) (enumerate-tree t))))

(define y (list 1 (list 2 (list 3 4)) 5))
(define x (list (list 1 2) (list 3 4)))
(length x); 3
(count-leaves x); 4
(count-leaves y); 5

; exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
	    (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(define y (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(accumulate-n + 0 y)

; exercise 2.37

(define mat (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define vec (list 2 2 2 2))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (accumulate-n * 1 (list x v))) m))

(matrix-*-vector mat vec)
;Value 35: ((2 4 6 8) (8 10 12 12) (12 14 16 18))

(define (transpose mat)
  (accumulate-n cons () mat))

(transpose mat)
; ((1 4 6) (2 5 7) (3 6 8) (4 6 9))

; mat:
((1 2 3 4)
 (4 5 6 6)
 (6 7 8 9))
; transpose of mat:
((1 4 6)
 (2 5 7)
 (3 6 8)
 (4 6 9))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (y)
	   (map (lambda (x)
		  (accumulate +
			      0
			      (accumulate-n *
					    1
					    (list y x))))
		mat))
	 mat)))

(define b (transpose mat))
(matrix-*-matrix mat b)
;((30 56 80) (56 113 161) (80 161 230))

; exercise 2.38 
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)
(fold-right / 1 (list 1 2 3)); 3/2
(fold-left / 1 (list 1 2 3)); 1/6
(fold-right list () (list 1 2 3)); (1 (2 (3 ())))
(fold-left list () (list 1 2 3)); (((() 1) 2) 3)

The operation must produce the same value regardless of the 
input order. For example addition and multiplication produce the same output:

(fold-right * 1 (list 1 2 3)); 6
(fold-left * 1 (list 1 2 3)); 6

(fold-right + 1 (list 1 2 3)); 7
(fold-left + 1 (list 1 2 3)); 7

; exercise 2.39

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))
(reverse (list 1 2 3))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) () sequence))
(reverse (list 1 2 3))
