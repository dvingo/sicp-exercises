;;    A       2    NA    16
;;    BOOM    1    SHA    3
;;    GET     2    YIP    9
;;    JOB     2    WAH    1
(define pairs '((NA 16) (YIP 9) (SHA 3) (JOB 2)
		(GET 2) (A 2) (WAH 1) (BOOM 1)))

(define rock-tree (generate-huffman-tree pairs))
	
(define message '(Get a job Sha na na na na na na na na Get a job 
  Sha na na na na na na na na
  Wah yip yip yip yip yip yip yip yip
  Sha boom))
(encode message rock-tree)

 (1 1 1 1 0 1 1 1 1 1 0 1 1 1 0 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 1 1 0 1 1 1 0 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 0 1 1 1 1 1 1 1)

; (define encoding (encode message rock-tree))
; (length encoding) ; 85


; (decode ' (1 1 1 1 0 1 1 1 1 1 0 1 1 1 0 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 1 1 0 1 1 1 0 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 0 1 1 1 1 1 1 1) rock-tree)

;Value 35: (get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip sha boom)


; A     000   NA  100
; BOOM  001   SHA 101
; GET   010   YIP 110
; JOB   011   WAH 111

; With a fixed length code, all symbols have a lenght of three.
; The input song is 35 symbols long, so the encoding using a 
; fixed length would be 35 * 3 = 105 characters long.
; 85/105 = x/100 = 0.810; 
; 100 - 81 = 19% larger.
