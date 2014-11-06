(let* ((x 3)
          (y (+ x 2))
          (z (+ x y 5)))
     (* x z))
(define (bindings exp) (cadr exp))
(define (body exp) (caddr exp))
(define (let*->nested-lets exp)
  (expand-lets (bindings exp) (body exp)))

(define (expand-lets bindings body)
  (if (null? bindings)
    body
    (let ((first (car bindings))
         (rest (cdr bindings)))
      (let->combination (list 'let
                               (cons first '())
                               (expand-lets rest body))))))
(let*->nested-lets y)

;; We can definitely add let* to the evaluator by utilizing
;; the existing let transformation. The insight here is that
;; these are purely syntactical transformations so the
;; syntax abstractions have the same semantics as the primitives
;; they transform into.
