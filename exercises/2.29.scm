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
