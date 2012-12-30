(define (p row col)
  (cond ((> col row) #f)
        ((or (= row 0) (= col 0) (= row col)) 1)
	(else (+ (p (- row 1) col)
		 (p (- row 1) (- col 1))))))
