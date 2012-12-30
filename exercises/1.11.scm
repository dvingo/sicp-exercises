(define (f n)
  (cond ((< n 3) n)
	(else (+ (f (- n 1))
		 (* 2 (f (- n 2)))
		 (* 3 (f (- n 3)))))))

f(1):  1
f(2):  2
f(3):  2 + 2(1)  + 3(0)
f(4):  4 + 2(2)  + 3(1)
f(5): 11 + 2(4)  + 3(2) : 25
f(6): 25 + 2(11) + 3(4) : 59
f(7): 59 + 2(25) + 3(11) : 142

; (display "a:")
; (display a) (newline)
;  (display "b:")
; (display b) (newline)
;  (display "c:")
; (display c) (newline)
; (display "curr: ") (display curr) (newline) (newline)

(define (f-iter a b c curr final)
  (if (= curr final)
      (+ a (* 2 b) (* 3 c))
      (f-iter (+ a (* 2 b) (* 3 c))
	      a
	      b
	      (+ curr 1)
	      final)))

(define (f n)
  (cond ((< n 3) n)
        ((= n 3) 4)
	(else (f-iter 4 2 1 4 n))))

(f-iter 2 1 0 3 5)
(f-iter 4 2 1 4 5)
(f-iter 11 4 2 5 5)
