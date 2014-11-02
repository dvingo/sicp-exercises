;; Define procedures that implement the term-list representation
;; described above as appropriate for dense polynomials.

;; The description on page 276:
;; The term lists of dense polynomials are most efficiently represented as
;; lists of the coefficients. For example, A above would be nicely
;; represented as (1 2 0 3 -2 -5). The order of a term in this representation
;; is the length of the sublist beginning with that term’s coefficient,
;; decremented by 1.

;; List of term-list procedures from page 277.

;; The only differing functions are adjoin term and first-term
;; which deal with the term-list just being a list of coefficients
;; and the order being the index of that term in the list.

(define (adjoin-term term term-list)
  (cons (coeff term) term-list))

(define (the-empty-termlist) '())

(define (first-term term-list) (make-term
                                (- (length term-list) 1)
                                (car term-list)))
(define (rest-terms term-list) (cdr term-list))
(define (emtpy-termlist? term-list) (null? term-list))

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))
