(define (=zero? poly)
  (apply-generic '=zero? poly))

(put '=zero? '(poly)
     (lambda (p) (=zero? (coefficient (first-term (term-list p))))))
