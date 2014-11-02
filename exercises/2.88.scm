;; Extend the polynomial system to include subtraction of polynomials.
;; (Hint: You may find it helpful to define a generic negation operation.)

(define (sub-poly p1 p2)
  (add-poly p1 (negation p2)))

;; We need to add support for scheme numbers, rational, imaginary, and polynomials.
;; The generic definition:
(define (negation x) (apply-generic 'negation x))

;; Scheme numbers: (inside install-scheme-number-package)
(put 'negation 'scheme-number
     (lambda (x) (tag (- x))))

;; Rational numbers: (inside install-rational-package)
(put 'negation 'rational
     (lambda (x) (tag (make-rat (- (numer x))
                                (denom x)))))

;; Complex numbers: (inside install-complex-package)
(put 'negation 'complex
     (lambda (z) (tag (make-from-real-imag
                       (- (real-part z))
                       (- (imag-part z))))))

;; Polynomial:
;; Here we model the format of mul-term-by-all-terms to negate
;; each term recursively.
(define (negation-poly p)
  (define (negate-all-terms L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t (first-term L)))
          (adjoin-term
           (make-term (order t) (negation (coeff t)))
           (negate-all-terms (rest-terms L))))))
  (negate-all-terms (term-list p)))
