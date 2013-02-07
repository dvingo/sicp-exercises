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
(define (reverse l)
  (if (null? (cdr l))
      l
      (append (reverse (cdr l)) (list (car l)))))
; accumulate form:
(define (reverse l)
  (define (helper li ac)
    (if (null? (cdr li))
	(cons (car li) ac)
      (helper (cdr li) (cons (car li) ac))))
  (helper (cdr l) (cons (car l) ())))
; TODO this returns incorrectly for (list 1 2 3 4)
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
; however in my code I relied upon the implementation of the
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
