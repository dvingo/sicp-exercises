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
      ;; probably just need to use append instead of cons here
	  (cons (deep-reverse (cdr l)) (list (deep-reverse (car l)))))))

(define x (list (list 1 2) (list 3 4)))
(reverse x)
; ((3 4) (1 2))
(deep-reverse x)
; ((4 3) (2 1))
