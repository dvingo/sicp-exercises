(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (define (helper a num-divisions)
    (cond ((not (= (remainder a 2) 0)) num-divisions)
	  (else (helper (/ a 2) (+ 1 num-divisions)))))
  (helper z 0))

(define (cdr z)
  (define (helper a num-divisions)
    (cond ((not (= (remainder a 3) 0)) num-divisions)
	  (else (helper (/ a 3) (+ 1 num-divisions)))))
  (helper z 0))
