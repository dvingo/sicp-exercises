;; In (eval)..
((let? exp) (let->combination exp env))

;; examples
;;
;;      (let ((first (car clauses))
;;            (rest (cdr clauses)))
;;            body)
;; (let ((first (car clauses)) (rest (cdr clauses))) body)
(define (let? exp) (tagged-list? exp 'let))
;; (let ((var1 (exp 1)) (var2 (exp 2))) body)
;; into->
;; ((lambda (var1 var2)
;;  body) (exp 1) (exp 2))
(define (k-vs exp) (cadr exp))
(define (var-list kvs) (map car kvs))
(define (val-list kvs) (map cadr kvs))
(define (let-body exp) (caddr exp))
(define (let->combination exp)
  (cons
    (make-lambda (var-list (k-vs exp))
                 (let-body exp))
    (val-list (k-vs exp))))



