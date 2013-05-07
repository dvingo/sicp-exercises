;; from 2.63:
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (element-of-set? x set)
  (cond ((null? set) #f)
	((= x (entry set)) #t)
	((< x (entry set))
	 (element-of-set? x (left-branch set)))
	((> x (entry set))
	 (element-of-set? x (right-branch set)))))
(define (adjoin-set x set)
  (cond ((null? set) (make-tree x () ()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))

;; this is not balanced
(define (union-set set1 set2)
  (define (helper li set)
    (if (null? li)
	set
	(adjoin-set (car li) (helper (cdr li) set))))
  (helper (tree->list-2 set1) set2))
    
(define (intersection-set set1 set2)
)

(define tree1 (list->tree '(7 10 11 12 13)))
(define tree2 (list->tree '(1 3 5 7 9 11)))

(union-set tree1 tree2 (5 (1 () (3 () ())) (9 (7 () ()) (11 (10 () ()) (13 (12 () ()) ())))))

(5 
 (1 () (3 () ())) 
 (9 (7 () ())
    (11 (10 () ())
	(13 (12 () ()) ()))))

                   5
                 /   \
                1     9
                 \   / \
                  3 7   11
                       /  \
                      10  13
                          /
                         12
