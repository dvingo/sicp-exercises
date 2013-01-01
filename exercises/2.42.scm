;(load "sequence-operations.scm") ; assumes scheme is started from top level dir of git repo

(define empty-board ())
(define (same-diag? first-pos second-pos)
  (and (= (abs (- (car second-pos) (car first-pos)))
	  (abs (- (cadr second-pos) (cadr first-pos))))
       (not (= (car second-pos) (car first-pos)))
       (not (= (cadr second-pos) (cadr first-pos)))))

; some tests:
; (same-diag? (list 3 2) (list 2 1))
; (same-diag? (list 2 1) (list 3 2))
; (same-diag? (list 2 1) (list 4 3))
; (same-diag? (list 1 1) (list 4 3))
; (same-diag? (list 4 3) (list 4 3))

(define (safe? k positions)
  (let ((k-pos (car (filter (lambda (pos) (= k (cadr pos))) positions))))
    (let ((same-row (filter (lambda (pos)
			      (and (= (car k-pos) (car pos)) 
				   (not (= (cadr k-pos) (cadr pos)))))
			    positions))
	  (same-diag (filter (lambda (pos) (same-diag? pos k-pos)) positions)))
      (and (null? same-diag) (null? same-row)))))

; some tests:
; (define positons (list (list 1 1) (list 3 2) (list 5 3)))
; (define k-pos (car (filter (lambda (pos) (= 3 (cadr pos))) positons)))
; (define same-row (car (filter (lambda (pos) (= 3 (cadr pos))) positons)))
; (define same-diag (filter (lambda (pos) (same-diag? pos k-pos)) positons))
; (not (= (cadr same-row) (cadr k-pos)))
; (safe? 3 positons)
 ;  (if there is a queen in the same diagonal return false)
 ; (display "positions: ")(display positions)(newline)
 ; (display "k-pos: ")(display k-pos)(newline))

(define (adjoin-position new-row k rest-of-queens)
  (append (list (list new-row k)) rest-of-queens))

; from the text
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row
				    k
				    rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))
