(define (=zero? num)
  (apply-generic '=zero? num1))

; Scheme numbers
(put '=zero? '(scheme-number)
     (lambda (x) (= x 0)))

(put '=zero? '(rational)
     (lambda (x) (= (numer x) 0)))

(put '=zero? '(complex)
     (lambda (x) (and (= (real-part x) 0)
		      (= (imag-part x) 0))))
