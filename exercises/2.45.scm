(define (split t1 t2)
  (lambda (painter n)
    (if (= n 0)
	painter
	(let ((smaller (split painter (- n 1))))
	  (t1 painter (t2 smaller smaller))))))
