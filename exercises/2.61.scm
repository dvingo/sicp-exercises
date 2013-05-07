; from the text
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((= x (car set)) #t)
	((< x (car set)) false)
	(else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      ()
       (let ((x1 (car set1)) (x2 (car set2)))
	 (cond ((= x1 x2)
		(cons x1
		      (intersection-set (cdr set1)
					(cdr set2))))
	       ((< x1 x2)
		(intersection-set (cdr set1) set2))
	       ((< x2 x1)
		(intersection-set set1 (cdr set2)))))))

(define (adjoin-set x set)
  ; using the same logic as element-of-set
  ; where we stop once we find the item that is greater than the one
  ; to insert
  (define (place-item item past-set search-set)
    (cond ((null? search-set) (append set (list item)))
	  ((> (car search-set) item)
	   (append past-set (cons item search-set)))
	  (else (place-item item
			    (append past-set (list (car search-set)))
			    (cdr search-set)))))
  (if (element-of-set? x set)
      set
      (place-item x () set)))

; tests
; (define s1 '(1 3 4 5 10 44 56 99))
; (define s2 '(3 22 43 44 55 80))
; (element-of-set? 11 s1)
; (element-of-set? 10 s1)

; (intersection-set s1 s2)

; (adjoin-set 11 s2); (3 11 22 43 44 55 80)
; (adjoin-set 91 s2); (3 22 43 44 55 80 91)
; (adjoin-set 56 s2); (3 22 43 44 55 56 80)
; (adjoin-set 3 s2); (3 22 43 44 55 80)
; (adjoin-set 1 s2); (1 3 22 43 44 55 80)
