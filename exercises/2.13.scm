; (load "2.12.scm")
(define a (make-center-percent 3 .2))
(define b (make-center-percent 2 .3))
; (percent (mul-interval a b)); = .2145214521452145
(define a (make-center-percent 3 .2))
(define b (make-center-percent 2 .5)); percent: .31147540932606564

; this looks like the approximate percentage tolerance of the product
; of two intervals is the average of the percentage tolerances of the factors.
(/ (+ .2 .3) 2); = .25
(/ (+ .2 .5) 2); = .35

