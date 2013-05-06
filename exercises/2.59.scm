; from the text:
(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) ())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

; actual question:
; running time is big-theta(n^2)
; include in the result only those elements in set1 that 
; are not included in set2, combined with all of set2
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((not (element-of-set? (car set1) set2))
	 (cons (car set1) (union-set (cdr set1) set2)))
	(else (union-set (cdr set1) set2))))

; test
;(define s1 '(1 2 3 4 5))
;(element-of-set? 3 s1)

;(define s2 '(a b c d 1 3))
;(union-set s1 ()); (1 2 3 4 5)
;(union-set s1 s2); (2 4 5 a b c d 1 3)
