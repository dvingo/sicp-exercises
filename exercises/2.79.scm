; Two numbers are equal when divided by one another they equal 1
; This also allows us to use all existing procedures
(define (equ num1 num2)
  (= (contents (apply-generic 'divide num1 num2)) 1))

; Use the number's existing type
(define (tag x)
  (attach-tag (type-tag x) x))

; Make our dispatch table aware of the new method
(put 'equ '(scheme-number scheme-number)
          (lambda (x y) (tag (equ x y))))

(put 'equ '(complex complex)
          (lambda (x y) (tag (equ x y))))

(put 'equ '(rational rational)
          (lambda (x y) (tag (equ x y))))


; This might be attempting to be too slick, might need to define
; a seperate procedure for each underlying type then have the generic
; procedure dispatch to those.

;; Dividing a a complex number by itself is 1:
;5 + 2i / 5 + 2i

;conjugate of the denom is 5 - 2i (causes the middle term to cancel

;((5 + 2i) / (5 + 2i)) * ((5 - 2i) / (5 - 2i))

;25 - 10i + 10i - 4i^2 = 25 + 4 = 29 / 29 = 1
