:- use_module(library(real)).

% R interface
:- instead_of <- (
       function(instead, of) :-
           return(instead)).

:- omit0 <- (
    function(expr) :-
        return(0)).

:- omit1 <- (
    function(expr) :-
        return(1)).

:- frac <- '`/`'.

:- dfrac <- '`/`'.

:- overline <- identity.

:- denoting <- (
       function(x, y, desc) :-
           return(y)).

:- format_tratio <- (
       function(x) :-
           sprintf(ifelse(abs(x) > 10, "%.1f", "%.2f"), x)).

:- sub <- (
    function(x, s) :-
        return(x)).
