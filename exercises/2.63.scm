;; Sets as binary trees

;; from the text:
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

;; Actual problem:
;; From the text:
(define (tree->list-1 tree)
  (if (null? tree)
      ()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1
		     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list
			     (right-branch tree)
			     result-list)))))
  (copy-to-list tree ()))

;; Trees from Figure 2.16:
(define tree1 (make-tree 7
			 (make-tree 3
				    (make-tree 1 () ())
				    (make-tree 5 () ()))
			 (make-tree 9 () (make-tree 11 () ()))))

(define tree2 (make-tree 3
			 (make-tree 1 () ())
			 (make-tree 7
				    (make-tree 5 () ())
				    (make-tree 9 () (make-tree 11 () ())))))

(define tree3 (make-tree 5
			  (make-tree 3 
				     (make-tree 1 () ())
				     ())
			  (make-tree 9
				     (make-tree 7 () ())
				     (make-tree 11 () ()))))

;; Tests
;(tree->list-1 tree1)
; (1 3 5 7 9 11)

;(tree->list-1 tree2)
;(1 3 5 7 9 11)

;(tree->list-1 tree3)
;(1 3 5 7 9 11)

;(tree->list-1 tree3)
;(1 3 5 7 9 11)

;(tree->list-2 tree1)
;(1 3 5 7 9 11)

;(tree->list-2 tree2)
;(1 3 5 7 9 11)

;(tree->list-2 tree3)
;(1 3 5 7 9 11)

; a:
; They are performing the same algorithm, the second one passes
; the returned list as the third argument, whereas the first
; just append the recursive calls directly.

; b:
;Each one results in a recursive call for the left and right branch
;yet the first algorithm uses append, whereas the second uses only cons.
;The append call will traverse the left branch which is about 
;half the nodes in the tree and thus has a running time of O(log(n))
;resulting in a running time of O(n log(n)) for the first algorithm, and
;a running time of O(n) for the second.
