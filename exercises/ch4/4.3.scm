;; Rewrite eval so that the dispatch is done in data- directed style.
;; Page 480
(define (eval exp env)
    (cond ((self-evaluating? exp) exp)
          ((variable? exp) (lookup-variable-value exp env))
          ((quoted? exp) (text-of-quotation exp))
          ((assignment? exp) (eval-assignment exp env))
          ((definition? exp) (eval-definition exp env))
          ((if? exp) (eval-if exp env))
          ((lambda? exp)
           (make-procedure (lambda-parameters exp)
                           (lambda-body exp)
                           env))
          ((begin? exp)
           (eval-sequence (begin-actions exp) env))
          ((cond? exp) (eval (cond->if exp) env))
          ((application? exp)
           (apply (eval (operator exp) env)
                  (list-of-values (operands exp) env)))
          (else
           (error "Unknown expression type - EVAL" exp))))


;; self-evaluating and variable? take the literal
;; themselves and not the car.
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
    (else
      (let ((proc (get 'eval (car exp))))
        (if proc
          (apply proc exp env) ;; supported syntax
          (if (application? exp) ;; procedure invocation
              (apply (eval (operator exp) env)
                     (list-of-values (operands exp) env)
              (error "Unknown expression type - EVAL" exp))))))))

(define (return-self exp env) exp)
(define (make-procedure-helper exp env)
   (make-procedure
     (lambda-parameters exp)
     (lambda-body exp)
     env))
(define (eval-sequence-helper exp env)
  (eval-sequence (begin-actions exp) env))
(define (eval-cond-helper exp env)
  (eval (cond->if exp) env))

(put 'eval 'quote text-of-quotation)
(put 'eval 'set! eval-assignment)
(put 'eval 'define eval-assignment)
(put 'eval 'if eval-if)
(put 'eval 'lambda make-procedure-helper)
(put 'eval 'begin eval-sequence-helper)
(put 'eval 'cond eval-cond-helper)
