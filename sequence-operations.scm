(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
     (cond ((null? sequence) ())
           ((predicate (car sequence))
            (cons (car sequence)
                  (filter predicate (cdr sequence))))
           (else (filter predicate (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low (enumerate-interval (+ low 1) high))))
