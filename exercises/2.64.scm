;; From the text:
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

;; Converts an ordered list to a balanced binary tree.
;; The result returned by partial-tree is a pair (formed with cons)
;; whose car is the constructed tree and whose cdr is the list
;; of elements not included in the tree.
(define (partial-tree elts n)
  (if (= n 0)
      (cons () elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree
				 (cdr non-left-elts)
				 right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree
		       this-entry left-tree right-tree)
		      remaining-elts))))))))

; a second attempt at 'a':
The algorithm continually calls 'left-result' halving
the input until there are none left. This fill out the left
half of the tree. By using half of n as the point to split
on this has the effect of creating the desired balanced tree.
Once the algorithm bottoms out on the left side of the input,
It recursively calls itself to build the right sub-tree.
After all the recursive steps are hit it builds up the output
tree bottom up by cons'ing together all the recursive sub-trees.


; a:
Partial tree description:
- set left-size to half the input size
- set left-result to a recursive call with the passed in elements and 
  n set to left-size.
- set left-tree to the result of the previous recursive call (left-result)
- set non-left-elts to the right side of the tree (the remaining elements
  from the previous call.
- set right-size to the original input less (left-size + 1)
  the plus one is for taking into account the entry node.
- set right-result to a recursive call with the remaining elements
  left over from the left-result call.

This is a recursive algorithm that continuously calls partial-tree
with half the input size passed to construct the left tree
and the remaining input size passed to construct the right tree.
In the base case an empty list is returned which will properly
construct a leaf node.
The algorithm builds up a list of (make-tree) calls where the leaf
nodes are constructed in the base case (when n == 0).


; (load "2.63.scm")

; just for fun:
(define l1 '(1 2 3 4 5 6 7 8 9 10 11 12 13))

; (partial-tree l1 5)
 ((3 
     (1 () (2 () ()))
     (4 () (5 () ())))
  6 7 8 9 10 11 12 13)

               3
             /   \
            1     4
             \     \
              2     5

;; actual problem
(list->tree '(1 3 5 7 9 11))

(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

                                5
                             /     \
                            1       9
                             \     / \ 
                              3   7  11


; b:
The order of growth in the number of steps required by list->tree
to convert a list of n elements is big-theta(log(n)).
This results from halving the input with each recursive call which is 
the distinguishing characteristic of logarithmic growth.
