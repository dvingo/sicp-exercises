(define (cont-frac n d k)
  (define (iter i holder)
    (if (= i k)
	holder
	(iter (+ i 1) (/ (n (- k i)) (+ (d (- k i)) holder)))))
  (iter 1 (/ (n k) (d k))))

(cont-frac (lambda (i) 1.0)
	   (lambda (i) 1.0)
	   10)

; k of 10 was too small, 100 was sufficient
; b (recursive process)
(define (cont-frac-rec n d k)
  (define (helper i)
    (cond ((= i k) (d i))
	  (else (+ (d (- i 1)) (/ (n i) (helper (+ i 1)))))))
  (/ (n 1) (helper 2)))

(cont-frac-rec (lambda (i) 1.0)
	       (lambda (i) 1.0)
	       100)
(/ 1.0 (helper 2))
(/ 1.0 (+ 1.0 (/ 1.0 (helper 3))))
...
(helper k) == (+ 1.0 (/ 1.0 1.0))
