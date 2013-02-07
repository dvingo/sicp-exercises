;Give combinations of cars and cdrs that will pick 7 from each of the following lists:
;(1 3 (5 7) 9)
;((7))
;(1 (2 (3 (4 (5 (6 7))))))

(define y (list 1 3 (list 5 7) 9))
(car (cdr (caddr y)))

(define y (list (list 7)))
(caar y)

(define y (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(cadr (cadr (cadr (cadr (cadr (cadr y))))))
