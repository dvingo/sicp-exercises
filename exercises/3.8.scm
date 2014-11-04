;; Define a simple procedure f such that evaluating (+ (f 0) (f 1))
;; will return 0 if the arguments to + are eval'd from L to R but
;; will return 1 if the args are eval'd from R to L.

(define f
  ((lambda (s)
    (lambda (x)
      (if (= s -1)
        (begin (set! s x) s)
        s))) -1))

(define f
  (let ((s -1))
    (lambda (x)
      (if (= s -1)
        (begin (set! s x) s)
        s))))

