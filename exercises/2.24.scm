1 ]=> (list 1 (list 2 (list 3 4)))

; (1 (2 (3 4)))

; ascii art of the box and pointer
; the numbers are pointers to numbers and the empty 
; brackets contain dots with the subsequent arrows coming
; out of them.
[1][]->[2][]->[3][]->[4][/]

; ascii art of the tree represented by the list
(1 (2 (3 4)))
      /\
     /  \
    1   (2 (3 4))
         /\
        /  \
       2   (3 4)
            /\
           /  \
          3    4
