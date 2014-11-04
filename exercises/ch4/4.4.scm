((and? exp) (eval-and exp env))
((or? exp) (eval-or exp env))

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

(define (eval-and exp env)
  (define (h args)
    (if (null? args)
      (last exp)
      (if (not (eval (car args) env))
        false
        (h (cdr args)))))
  (h (cdr exp)))

(define (eval-or exp env)
  (define (h args)
    (if (null? args)
      false
      (if (eval (car args) env)
        (car args)
        (h (cdr args)))))
  (h (cdr exp)))

;; As derived expressions.
((and? exp) eval (and->if exp) env)
((or? exp) eval (or->if exp) env)

(define (and-clauses exp) (cdr exp))
(define (and->if exp)
  (expand-and-clauses (and-clauses exp)))

(define (expand-and-clauses clauses)
  (define (append-if args)
    (let ((first (car args))
          (rest (cdr args)))
      (if (null? rest)
        first
        (make-if first (append-if rest) false))))
  (if (null? clauses)
      true
      (append-if clauses)))

(define (or-clauses exp) (cdr exp))
(define (or->if exp)
  (expand-or-clauses (or-clauses exp)))

(define (expand-or-clauses clauses)
  (define (append-if args)
    (let ((first (car args))
          (rest (cdr args)))
      (if (null? rest)
        (make-if first first false)
        (make-if first first (append-if rest)))))
  (if (null? clauses)
      false
      (append-if clauses)))
