;; From the text:
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))))
     segment-list)))

; Drawing in MIT-Scheme References:
;http://sicp.ai.mit.edu/Spring-2005/manuals/scheme-7.5.5/doc/scheme_18.html#SEC192
; http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Graphics.html#Graphics
(define d (make-graphics-device (car (enumerate-graphics-types))))
(graphics-operation d 'set-foreground-color "red")
(graphics-operation d 'set-background-color "blue") 
(graphics-operation d 'fill-circle 0 0 0.1)
(graphics-draw-line d 0 0 5 5)
(graphics-close d)

