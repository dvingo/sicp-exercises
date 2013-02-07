; (load "2.2.scm")
; exercise 2.3

; takes two points top-left and bottom-right
(define (make-rec top-left bottom-right)
  (cons (cons top-left 
	(make-point (x-point bottom-right) (y-point top-left)))
	(cons (make-point (x-point top-left) (y-point bottom-right))
	      (make-point (x-point bottom-right) (y-point bottom-right)))))

(make-rec (make-point 2 4) (make-point 4 0))

; four points of the rectangle
(define (tl rec)
  (car (car rec)))
(define (tr rec)
  (cdr (car rec)))
(define (bl rec)
  (car (cdr rec)))
(define (br rec)
  (cdr (cdr rec)))

; version 2:
(define (make-rec top-segment left-segment)
  (cons top-segment left-segment))

(define top-segg (make-segment (make-point 2 4) (make-point 4 4)))
(define left-segg (make-segment (make-point 2 4) (make-point 2 0)))

(define my-rec (make-rec top-segg left-segg))

(define (tl rec)
  (start-segment (car rec)))
(define (tr rec)
  (end-segment (car rec)))
(define (bl rec)
  (end-segment (cdr rec)))
(define (br rec)
  (make-segment (x-point (end-segment (car rec)))
		(y-point (end-segment (cdr rec)))))
  
(define (top-seg rec)
  (make-segment (tl rec) (tr rec)))
(define (bot-seg rec)
  (make-segment (bl rec) (br rec)))
(define (left-seg rec)
  (make-segment (tl rec) (bl rec)))
(define (right-seg rec)
  (make-segment (tr rec) (br rec)))
(define (square x) (* x x))
(define (length segment)
  (let ((x1 (x-point (start-segment segment)))
	(x2 (x-point (end-segment segment)))
	(y1 (y-point (start-segment segment)))
	(y2 (y-point (end-segment segment))))
    (sqrt (+ (square (- x2 x1)) (square (- y2 y1))))))

(define (rect-perimeter rec)
  (+ (length (top-seg rec))
     (length (bot-seg rec))
     (length (left-seg rec))
     (length (right-seg rec))))

(define (rect-area rec)
  (* (length (top-seg rec))
     (length (left-seg rec))))
