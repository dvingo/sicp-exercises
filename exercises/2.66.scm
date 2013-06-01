; from the text:
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
	((equal? given-key (key (car set-of-records)))
	 (car set-of-records))
	(else (lookup given-key (cdr set-of-records)))))


;; Implement the lookup procedure for the case where
;; the set of records is structured as a binary tree,
;; ordered by the numerical values of the keys.

; (load "2.65.scm")
; this is essentially 'element-of-set' but using 
; the 'key' function instead of just straight 'entry'
; it is the same, binary search
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
	((= x (key (entry set-of-records))) #t)
	((< x (key (entry set-of-records)))
	 (element-of-set? x (left-branch set-of-records)))
	((> x (key (entry set)))
	 (element-of-set? x (right-branch set-of-records)))))
