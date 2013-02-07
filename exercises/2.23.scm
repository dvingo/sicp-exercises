(define (for-each f items)
  (define (helper i)
    (f i)
    (for-each f (cdr items)))
  (if (null? items)
      #t
      (helper (car items))))

; (for-each (lambda (x) (newline) (display x)) (list 57 321 88))
;57
;321
;88
;Value: #t
