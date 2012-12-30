; Recursive process defined on pg 64
(define (+ a b)
  (if (= a 0) b (inc (+ (dec a) b))))

(+ 4 5):

(if (= 4 0) 5 (inc (+ (dec 4) 5)))
(inc (+ (dec 4) 5))
(inc (+ 3 5))
(inc (inc (+ (dec 3) 5)))
(inc (inc (inc (+ (dec 2) 5))))
(inc (inc (inc (inc (+ (dec 1) 5)))))
;(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

The process is recursive because these are deferred operations.

(define (+ a b)
  (if (= a 0) b (+ (dec a) (inc b))))

(+ 4 5):
(+ (dec 4) (inc 5))
(+ (dec 3) (inc 6))
(+ (dec 2) (inc 7))
(+ (dec 1) (inc 8))
(+ 0 9)
9
This process is iterative because the state can be summarized by a fixed number of state variables, together with a fixed rule that describes how the state variables should be updated as the process moves from state to state and an (optional) end test that specifies conditions under which the process should terminate.  This is a linear iterative process because the number of steps required grows linearly with n.
