; (load "2.7.scm")
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent center tol)
  (make-interval (- center tol) (+ center tol)))

(define (percent interval)
  (/ (width interval) (center interval)))

(define x (make-interval 1.3 3.4))
(define y (make-interval 2.3 5.8))

; (center x); 2.35
; (width x); 1.0499999999999998
; (make-center-width 2.35 1.0499999999999998); (1.3000000000000003 . 3.4)
; (percent x) ;.4468085106382978

;  (/ (width x) (center x))
;Value: .4468085106382978

; (* .4468 2.35): 1.04998

; (+ 1.04998 2.35) : 3.3999800000000002

; (- 2.35 1.04998) : 1.3000200000000002
