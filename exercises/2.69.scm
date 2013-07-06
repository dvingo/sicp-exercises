(load "huffman-codes.scm")

;; (make-leaf-set '((A 4) (B 2) (C 1) (D 1)))
;; returns: ((leaf d 1) (leaf c 1) (leaf b 2) (leaf a 4))
;; (define l-set (make-leaf-set '((A 4) (B 2) (C 1) (D 1))))


;; Merge the last two elements recursively
;; until the recursion bottoms out and just return the combined elements.
(define (successive-merge leaf-set)
  (define (merge-helper next-leaf remaining-input return-list)
    (cond ((null? remaining-input) (make-code-tree next-leaf return-list))
	  (else ;(debug-print "next-leaf: " next-leaf)
		;(debug-print "remaining-input: " remaining-input)
		;(debug-print "return-list: " return-list)
		(merge-helper (car remaining-input)
			      (cdr remaining-input)
			      (make-code-tree next-leaf return-list)))))
  (merge-helper (cadr leaf-set) (cddr leaf-set) (car leaf-set)))

(successive-merge l-set)
	  
(
 (leaf a 4)
 ((leaf b 2)
  ((leaf c 1) (leaf d 1)
   (c d) 2)
   (b c d) 4)
   (a b c d) 8)

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))

; Test
; sample-tree
;Value 25: ((leaf a 4) ((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4) (a b d c) 8)

; l-set is defined above
; (successive-merge l-set)
; Value 26: ((leaf a 4) ((leaf b 2) ((leaf c 1) (leaf d 1) (c d) 2) (b c d) 4) (a b c d) 8)


; Test successive-merge
; (successive-merge (make-leaf-set '((A 4) (B 2) (C 1) (D 1))))

; Test generate-huffman-tree
(define pairs  '((A 4) (B 2) (C 1) (D 1)))
(generate-huffman-tree pairs)
