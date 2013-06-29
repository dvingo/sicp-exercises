(load "huffman-codes.scm")

(define (contains? set member)
  (cond ((null? set) #f)
	((eq? (car set) member) #t)
	(else (contains? (cdr set) member))))

; (contains? '(a b d c) 'a)
; (contains? '(a b d c) 'x)

(define (encode-symbol symbol tree)
  (define (helper ret-list remaining-tree)
    (cond ((null? remaining-tree)
	   (error "symbol not in tree - ENCODE-SYMBOL" symbol))
	  ((leaf? remaining-tree)
	   (display "returning from enc-symb: ")(display ret-list)(newline)
	   ret-list)
	  ((contains? (symbols (left-branch remaining-tree)) symbol)
	   (cons 0 (helper ret-list (left-branch remaining-tree))))
	  ((contains? (symbols (right-branch remaining-tree)) symbol)
	   (cons 1 (helper ret-list (right-branch remaining-tree))))
	  (else (error "symbol not in tree - ENCODE-SYMBOL" symbol))))
  (helper '() tree))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))
(encode '(a d a b b c a) sample-tree)
