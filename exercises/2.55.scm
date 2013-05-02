(car ''abracadabra)
expands to 
(car (quote (quote abracadabra)))
by the interpreter, so the first element of the resulting list
is just quote.
