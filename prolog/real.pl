:- use_module(library(real)).

% R interface
:- instead_of <- (
       function(instead, of) :-
       return(instead)).

:- frac <- '`/`'.

:- dfrac <- '`/`'.

:- overline <- identity.

:- denoting <- (
       function(x, y, desc) :-
       return(y)).
