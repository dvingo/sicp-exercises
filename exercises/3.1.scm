(define (make-accumulator initial-val)
  (lambda (next-num)
    (begin (set! initial-val (+ initial-val next-num))
           initial-val)))
