(define (d i)
  (cond ((not (= (remainder (+ i 1) 3) 0)) 1)
	(else (* 2 (/ (+ i 1) 3)))))
(cont-frac (lambda (i) 1.0) d 100)
(cont-frac-rec (lambda (i) 1.0) d 100)

; e = 2.71828182846
; e - 2 = .71828182846
