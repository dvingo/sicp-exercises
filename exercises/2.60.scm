; exact same implementation as non-duplicated representation
; 
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((equal? x (car set)) #t)
	(else (element-of-set? x (cdr set)))))

; (define s1 '(2 3 2 1 3 2 2))
; (element-of-set? '2 s1)
; (element-of-set? '1 s1)
; (element-of-set? '3 s1)

; blindly add the element
; this would have better running time
; as it returns in constant time
(define (adjoin-set x set)
  (cons x set))
; (adjoin-set 10 s1)


; add every element of set1 to set2
; running time is big-theta((length(set1))
(define (union-set set1 set2)
  (if (null? set1)
      set2
      (cons (car set1) (union-set (cdr set1) set2))))
; (define s1 '(2 3 2 1 3 2 2))
; (define s2 '(a b c d 1 3))
; (union-set s1 ());  (2 3 2 1 3 2 2)
; (union-set s1 s2); (2 3 2 1 3 2 2 a b c d 1 3)     


;this exhibits the same running time as the first
; due to the need to check all of set2 for each element of set1
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) ())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

; (intersection-set s1 s2)
; (2 3 2 1 3 2 2 a b c d 1 3)

; If you ever had lots and lots of memory yet a slow processor
; you would favor the duplicate representation over the unique representation
; 
; element-of-set remains at O(n)
; union-set goes from O(n^2) to O(n)
; intersection-set remains the same at O(n^2)
; adjoin-set goes from O(n) to O(1)
; so you trade increased memory footprint for decreased running time
