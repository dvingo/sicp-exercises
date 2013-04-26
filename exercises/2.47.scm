(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define orig (make-vect 0 0))
(define e1 (make-vect 8 3))
(define e2 (make-vect 10 6))
(define f (make-frame orig e1 e2))
(origin-frame f)
(edge1-frame f)
(edge2-frame f)

; Second implementation

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cddr frame))

(define orig (make-vect 0 1))
(define e1 (make-vect 8 4))
(define e2 (make-vect 13 6))
(define f (make-frame orig e1 e2))
(origin-frame f)
(edge1-frame f)
(edge2-frame f)
