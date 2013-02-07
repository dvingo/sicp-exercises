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
