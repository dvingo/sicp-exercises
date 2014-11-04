(define (list-of-values exps env)
    (if (no-operands? exps)
      '()
      (cons-l-to-r (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;; The thought here is that by constructing a temporary
;; list first the evaluation order that we want will
;; be achieved.
(define (cons-l-to-r f r)
  (let ((first-list (cons f '())))
      (append first-list r)))

(define (cons-r-to-l f r)
  (let ((rest-list (cons '() r)))
    (cons f (cdr rest-list))))
