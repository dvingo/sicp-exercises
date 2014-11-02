(define (equ? num1 num2)
  (apply-generic 'equ? num1 num2))

; Ordinary numbers (scheme primitives)
(put 'equ? '(scheme-number scheme-number)
     (lambda (x y) (= x y)))

; Rational numbers - does not simplify
(put 'equ? '(rational rational)
     (lambda (x y) (and (= (numer x) (numer y))
                        (= (denom x) (denom y)))))

; Complex numbers
(put 'equ? '(complex complex)
     (lambda (x y) (and (= (imag-part x) (imag-part y))
                        (= (real-part x) (real-part y)))))
