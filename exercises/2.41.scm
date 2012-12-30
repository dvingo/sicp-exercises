(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
     (cond ((null? sequence) nil)
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

(define (list-sum-equal-n-gen n)
  (lambda (li) (= (+ (car li) (cadr li) (caddr li)) n)))

(define (unique-triplet n)
  (flatmap (lambda (i)
	     (flatmap (lambda (j)
			(map (lambda (k) (list k j i))
			     (enumerate-interval 1 (- j 1))))
		      (enumerate-interval 1 (- i 1))))
	     (enumerate-interval 1 n)))

(define (sum-to-given n s)
  (filter (list-sum-equal-n-gen s)
	  (unique-triplet n)))
