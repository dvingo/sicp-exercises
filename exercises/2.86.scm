;; Suppose we want to handle complex numbers whose real parts, imaginary parts,
;; magnitudes, and angles can be either ordinary numbers, rational numbers,
;; or other numbers we might wish to add to the system. Describe and implement the
;; changes to the system needed to accommodate this. You will have to define
;; operations such as sine and cosine that are generic over ordinary numbers
;; and rational numbers.

;; These changes should only affect the complex number package and have
;; no exposure to the rest of the apply-generic system.

;; This is an example of nested dispatch on type:
;;  complex -> {rectangular, polar} -> {underlying number type (scheme, rational, other...)}.

;; The changes needed to be made to the system are adding dispatches on type
;; to the polar and rectangular complex number tables.

;; From pages 244, 245:
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
	     (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag ’rectangular x))
  (put ’real-part ’(rectangular) real-part)
  (put ’imag-part ’(rectangular) imag-part)
  (put ’magnitude ’(rectangular) magnitude)
  (put ’angle ’(rectangular) angle)
  (put ’make-from-real-imag ’rectangular
	(lambda (x y) (tag (make-from-real-imag x y))))
  (put ’make-from-mag-ang ’rectangular
	(lambda (r a) (tag (make-from-mag-ang r a))))
  ’done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
	  (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag ’polar x))
  (put ’real-part ’(polar) real-part)
  (put ’imag-part ’(polar) imag-part)
  (put ’magnitude ’(polar) magnitude)
  (put ’angle ’(polar) angle)
  (put ’make-from-real-imag ’polar
	(lambda (x y) (tag (make-from-real-imag x y))))
  (put ’make-from-mag-ang ’polar
	(lambda (r a) (tag (make-from-mag-ang r a))))
  ’done)

;; Looking at these tables, we will need functions that work on generic input
;; for: addition, multiplication, square, sqrt, atan, cos, sin.
(define (generic-square x) (apply-generic 'generic-square x))
(define (generic-sqrt x) (apply-generic 'generic-sqrt x))
(define (generic-atan x) (apply-generic 'atan x))
(define (generic-cos x) (apply-generic 'cos x))
(define (generic-sin x) (apply-generic 'sin x))
(define (add x y) (apply-generic 'add x y))
(define (mul x y) (apply-generic 'mul x y))

;; Then we need to add support for these functions for scheme and rational
;; numbers.
;; Inside the install-rational-package on pg 254 we add:
(define (square-rat x)
  (make-rat (square (numer x)) (square (denom x))))
(define (sqrt-rat x)
  (sqrt (/ (numer x) (denom x))))
(define (atan-rat x)
  (atan (/ (numer x) (denom x))))
(define (cos-rat x)
  (cos (/ (numer x) (denom x))))
(define (sin-rat x)
  (sin (/ (numer x) (denom x))))
;; add and mul are already in the text.

(put 'generic-square 'rational (lambda (x) (tag square-rat x)))
(put 'generic-sqrt 'rational (lambda (x) (tag sqrt-rat x)))
(put 'generic-atan 'rational (lambda (x) (tag atan-rat x)))
(put 'generic-cos 'rational (lambda (x) (tag cos-rat x)))
(put 'generic-sin 'rational (lambda (x) (tag sin-rat x)))

;; Then the same for scheme numbers:
(define (square-scheme-number x) (square x))
(define (sqrt-scheme-number x) (sqrt x))
(define (atan-scheme-number x) (atan x))
(define (cos-scheme-number x) (cos x))
(define (sin-scheme-number x) (sin x))
;; add and mul are already in the text.

(put 'generic-square 'scheme-number (lambda (x) (tag square-scheme-number x)))
(put 'generic-sqrt 'scheme-number (lambda (x) (tag sqrt-scheme-number x)))
(put 'generic-atan 'scheme-number (lambda (x) (tag atan-scheme-number x)))
(put 'generic-cos 'scheme-number (lambda (x) (tag cos-scheme-number x)))
(put 'generic-sin 'scheme-number (lambda (x) (tag sin-scheme-number x)))

;; The functions in the tables that call these are:
;; For rectangular: magnitude, angle, make-from-mag-ang.
;; For polar: real-part, imag-part, make-from-real-imag.

;; Their new forms for rectangular are:
;; *note* I apply the generic- prefix to not conflict with
;; the native scheme function names.
(define (magnitude z)
  (sqrt (add (generic-square (real-part z))
	   (generic-square (imag-part z)))))
(define (angle z)
  (generic-atan (imag-part z) (real-part z)))
(define (make-from-mag-ang r a)
  (cons (mul r (generic-cos a)) (mul (generic-sin a))))

;; The new polar function definitions:
(define (real-part z)
  (mul (magnitude z) (generic-cos (angle z))))
(define (imag-part z)
  (mul (magnitude z) (generic-sin (angle z))))
(define (make-from-real-imag x y)
  (cons (generic-sqrt (add (generic-square x) (generic-square y))
		      (generic-atan y x))))
