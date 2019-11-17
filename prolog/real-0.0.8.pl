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

:- format_tratio <- (
       function(x) :-
           s = sprintf(ifelse(abs(x) > 10, '%.1f', '%.2f'), x)
           return(s)).
