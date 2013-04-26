;(load "2.46.scm")
(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

; Tests
;(define v1 (make-vect 0 0))
;(define v2 (make-vect 8 12))

;(define seg (make-segment v1 v2))
;(start-segment seg)
;(end-segment seg)


