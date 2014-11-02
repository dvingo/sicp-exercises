page 258
type-tag
contents
attach-tag


; original
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum - TYPE-TAG" datum)))

; new
(define (type-tag datum)
  (cond ((number? datum) datum)
        ((pair? datum) (car datum))
        (else
         (error "Bad tagged datum - TYPE-TAG" datum))))

; original
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum - CONTENTS" datum)))

; new
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (error "Bad tagged datum - CONTENTS" datum)))

;original
(define (attach-tag type-tag contents)
     (cons type-tag contents))

; new
(define (attach-tag type-tag contents)
  (cond ((number? contents) contents)
        ((symbol? contents)
         (cons type-tag contents))))

(put 'add '(scheme-number scheme-number)
     (lambda (x y) (tag (+ x y))))
