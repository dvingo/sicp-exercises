; given from the text:
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (cond ((>= start end) #t)
	((timed-prime-test start)
	 (search-for-primes (+ start 2) end))))

; runtime doesn't work on mit-scheme
; (search-for-primes 10000000001 10000000501))
10000000019 *** .17999999999999972
10000000033 *** .18000000000000016
10000000061 *** .18999999999999995

(search-for-primes 100000000001 100000000501))
;(sqrt 100000000001) == 316227.76601841906
100000000003 *** .5700000000000003
100000000019 *** .5700000000000003
100000000057 *** .5800000000000001
