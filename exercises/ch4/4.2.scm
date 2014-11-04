a. By putting the `application?` check before `assignment?` the evaluator
will treat all `(define ...)` and `(set! ...)` statements as
procedure calls which will obviously fail (as hinted at in the question).

b. The relevant syntax is defined in the `application?` procedure.
Instead of:

    (define (application? exp) (pair? exp))

We now need:

    (define (application? exp)
      (tagged-list? exp 'call))

Then we can put the application? check before the assignment?.
