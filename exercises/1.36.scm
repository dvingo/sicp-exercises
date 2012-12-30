(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display "guess: ")
    (display guess) (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define x-x
  (fixed-point (lambda (y) (/ (log 1000) (log y))) 2.0))
; console output
(define x-x
  (fixed-point (lambda (y) (/ (log 1000) (log y))) 2.0))
guess: 2.
guess: 9.965784284662087
guess: 3.004472209841214
guess: 6.279195757507157
guess: 3.759850702401539
guess: 5.215843784925895
guess: 4.182207192401397
guess: 4.8277650983445906
guess: 4.387593384662677
guess: 4.671250085763899
guess: 4.481403616895052
guess: 4.6053657460929
guess: 4.5230849678718865
guess: 4.577114682047341
guess: 4.541382480151454
guess: 4.564903245230833
guess: 4.549372679303342
guess: 4.559606491913287
guess: 4.552853875788271
guess: 4.557305529748263
guess: 4.554369064436181
guess: 4.556305311532999
guess: 4.555028263573554
guess: 4.555870396702851
guess: 4.555315001192079
guess: 4.5556812635433275
guess: 4.555439715736846
guess: 4.555599009998291
guess: 4.555493957531389
guess: 4.555563237292884
guess: 4.555517548417651
guess: 4.555547679306398
guess: 4.555527808516254
guess: 4.555540912917957

; using average damping
(define x-x
  (fixed-point (lambda (y) (/ (+ y (/ (log 1000) (log y))) 2)) 2.0))
; console output:
1 ]=> (define x-x
  (fixed-point (lambda (y) (/ (+ y (/ (log 1000) (log y))) 2)) 2.0))
guess: 2.
guess: 5.9828921423310435
guess: 4.922168721308343
guess: 4.628224318195455
guess: 4.568346513136242
guess: 4.5577305909237005
guess: 4.555909809045131
guess: 4.555599411610624
guess: 4.5555465521473675
