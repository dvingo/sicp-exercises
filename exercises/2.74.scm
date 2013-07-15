; a.

(define (get-record employee-name personnel-file division-name)
  ((get 'get-record division-name)
   employee-name personnel-file))

; Each division should be structured the same way they already are,
; they just need to have their division name associated with them.

; b.
(define (get-salary employee-name personnel-file division-name)
  ((get 'get-salary division-name)
   employee-name personnel-file))

; Same as above the record structure doesn't matter as long as each
; division provides an entry procedure in the table the 
; 'get-salary' operation, with their division-name as the type.

; c
(define (find-employee-record employee-name division-files)
  (map (lambda (x) (get-record employee-name x division-name)) division-files))

; d 
; The only changes needed are the for the new company to add
; entries in the table for the above three procedures.
