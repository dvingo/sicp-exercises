; linear algorithm:
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))
(define (double n) (+ n n))
(define (halve n) (/ n 2))
; logarithmic algorithm
(define (fast* a b)
  (cond ((= b 1) (+ a 1))
	((= b 2) (double a))
	((even? b) (+ (fast* a (halve b)) (fast* a (halve b))))
	(else (+ a (fast* a (- b 1))))))

(fast* 3 3)
(+ 3 (fast* 3 2))
(+ 3 (double 3))
(+ 3 6)
9

(fast* 2 5)
(+ 2 (fast* 2 4))
(+ 2 (+ (fast* 2 2) (fast* 2 2)))
(+ 2 (+ (double 2) (double 2)))
(+ 2 (+ 4 4))
(+ 2 8)
10
