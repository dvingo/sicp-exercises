(define (double f)
  (lambda (x) (f (f x))))

(define (inc x)
  (+ x 1))

(define (add-two x)
  ((double inc) x))

(((double (double double)) inc) 5) ; 21

(((double double) inc) 1) ; 5
(((double double) inc) 2) ; 6
(((double double) inc) 3) ; 7
(((double double) inc) 4) ; 8
(((double double) inc) 5) ; 9

(((double (double double)) inc) 1) ; 17
(((double (double double)) inc) 2) ; 18 
(((double (double double)) inc) 3) ; 19
(((double (double double)) inc) 4) ; 20
(((double (double double)) inc) 5) ; 21
(((double (double double)) inc) 6) ; 22
