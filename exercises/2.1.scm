; Chapter 2

; cons stands for construct.  The names car and cdr derive from the original 
; implementation of Lisp on the IBM 704.  That machine had an addressing scheme
; that allowed one to reference the "address" and "decrement" parts of a
; memory loction.  Car stands for "Contents of Address part of Register"
; and cdr (pronounced "could-er") stands for "Contents of Decrement part of
; Register."

; exercise 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((and (negative? n) (negative? d))
	   (cons (/ (* -1 n) g) (/ (* -1 d) g)))
	  ((and (negative? n) (positive? d))
	   (cons (/ n g) (/ d g)))
	  ((and (positive? n) (negative? d))
	   (cons (/ (* -1 n) g) (/ (* -1 d) g)))
	  (else (cons (/ n g) (/ d g))))))
