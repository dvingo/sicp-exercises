; exercise 2.18
(define (reverse l)
  (if (null? (cdr l))
      l
      (append (reverse (cdr l)) (list (car l)))))
; or with cons and accumulation:
(define (reverse l)
  (define (helper li ac)
    (if (null? (cdr li))
	(cons (car li) ac)
      (helper (cdr li) (cons (car li) ac))))
  (helper (cdr l) (cons (car l) ())))

; (reverse (list 1 2 3 4)); (4 3 2 1)
