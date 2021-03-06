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

(define (element-of-ordered-set? x set)
  (cond ((null? set) #f)
	((= x (car set)) #t)
	((< x (car set)) #f)
	(else (element-of-ordered-set? x (cdr set)))))

(define (adjoin-ordered-set x set)
  ;; using the same logic as element-of-set
  ;; where we stop once we find the item that is greater than the one
  ;; to insert
  (define (place-item item past-set search-set)
    (cond ((null? search-set) (append set (list item)))
	  ((> (car search-set) item)
	   (append past-set (cons item search-set)))
	  (else (place-item item
			    (append past-set (list (car search-set)))
			    (cdr search-set)))))
  (if (element-of-ordered-set? x set)
      set
      (place-item x () set)))


; (load "2.63.scm")
(define (union-set set1 set2)
  (define (unique-list li set ret-list)
    ;; uses helper adjoin-ordered-set to create a unique 
    ;; list representation of the union of set1 and set2
    (cond ((null? li) ret-list)
	  ((not (element-of-set? (car li) set))
	   (unique-list (cdr li) set (adjoin-ordered-set (car li) ret-list)))
	  (else (unique-list (cdr li) set ret-list))))
  ;; Create a list of ordered unique elements of set1 and set2
  ;; tree->list-2 has a linear growth running time
  (define the-set (unique-list (tree->list-2 set1) set2 (tree->list-2 set2)))
  ;; Convert that list into a balanced binary tree
  (list->tree the-set))

(define (intersection-set set1 set2)
  (define (intersection-list li set ret-list)
    ;; uses helper adjoin-ordered-set to create the interesection of li and set
    (cond ((null? li) ret-list)
	  ((element-of-set? (car li) set)
	   (intersection-list (cdr li) set (adjoin-ordered-set (car li) ret-list)))
	  (else (intersection-list (cdr li) set ret-list))))
  ;; Create a list of ordered unique elements of set1 and set2
  ;; tree->list-2 has a linear growth running time
  (define the-set (intersection-list (tree->list-2 set1) set2 '()))
  ;; Convert that list into a balanced binary tree
  (list->tree the-set))
  

(define tree1 (list->tree '(7 10 11 12 13)))
(define tree2 (list->tree '(1 3 5 7 9 11)))

(union-set tree1 tree2)

(9 (3 (1 () ()) (5 () (7 () ()))) (11 (10 () ()) (12 () (13 () ()))))
(9 
 (3
  (1 () ())
  (5 () (7 () ())))
 (11
  (10 () ())
  (12 () (13 () ()))))

                    9
                 /     \
                3       11
               / \      / \
              1   5   10  12
                            \
                            13


(intersection-set tree1 tree2)

;Value 4: (7 () (11 () ()))

                        7
                          \
                           11

(define tree3 (list->tree '(1 7 10 11 12 13)))
(define tree4 (list->tree '(1 3 5 7 9 11)))

(intersection-set tree3 tree4)
;Value 6: (7 (1 () ()) (11 () ()))

                        7
                      /   \
                     1     11
