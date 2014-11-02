;; Suppose we want to have a polynomial system that is efficient for both sparse
;; and dense polynomials... Redesign the polynomial system to implement
;; this generalization.

;; Similar to complex numbers, we will add a second layer of tagging to the
;; representation of the polynomial system.

;; page 256 for complex numbers.

;; page. 272 for polynomials.

(define (install-dense-poly-package)
  ;; internal procedures
  (define (adjoin-term term term-list)
    (cons (coeff term) term-list))
  (define (first-term term-list) (make-term
                                  (- (length term-list) 1)
                                  (car term-list)))

  ;; install functions into the system.
  (put 'adjoin-term 'dense adjoin-term)
  (put 'first-term 'dense first-term))

(define (install-sparse-poly-package)
  ;; internal procedures
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (first-term term-list) (car term-list))

  ;; install functions into the system.
  (put 'adjoin-term 'sparse adjoin-term)
  (put 'first-term 'sparse first-term))

(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))

  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  ;; term list functions common to both representations.
  (define (the-empty-termlist) '())
  (define (rest-terms term-list) (cdr term-list))
  (define (emtpy-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  ;; This is the only added level of indirection needed
  ;; as the rest of the term system is the same for both
  ;; representations.
  (define (make-dense-term order coeff)
    (attach-tag 'dense (make-term order coeff)))
  (define (make-sparse-term order coeff)
    (attach-tag 'sparse (make-term order coeff)))
  (define (adjoin-term term term-list)
    ((get 'adjoin-term (type-tag term)) term term-list))
  (define (first-term term-list)
    ((get 'first-term (type-tag term)) term-list)))

;; The type of polynomial will have to be specified at construction
;; time and passed down to make-term inside the polynomial functions.
;; add-terms, mult-terms etc are internal to the polynomial package so you
;; will need a different version for each.
