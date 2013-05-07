; The following is big-theta(n)
; for the same reasons as intersection-set from the section in the text:

; To estimate the number of steps required by this process,
; observe that at each step we reduce the intersection problem to
; computing intersections of smaller sets - removing the first element
; from set1 or set2 or both. Thus, the number of steps required is at
; most the sum of the sizes of set1 and set2, rather than the product
;  of the sizes as with the unordered representation.
; This is big-theta(n) growth rather than big-theta(n^2) - 
; a considerable speedup, even for sets of moderate size.

(define (union-set set1 set2)
  (cond ((null? set2) set1)
	((null? set1) set2)
	((= (car set1) (car set2)) 
	 (cons (car set1) (union-set (cdr set1) (cdr set2))))
	((< (car set1) (car set2))
	 (cons (car set1) (union-set (cdr set1) set2)))
	((> (car set1) (car set2))
	 (cons (car set2) (union-set set1 (cdr set2))))))

; (define s1 '(1 3 4 5 10 44 56 99))
; (define s2 '(3 22 43 44 55 80))
; (union-set s1 s2) ;  (1 3 4 5 10 22 43 44 55 56 80 99)
; (union-set s2 s1); (1 3 4 5 10 22 43 44 55 56 80 99)
; (union-set '(1 3) '(2 3 4 5)) ; (1 2 3 4 5)
