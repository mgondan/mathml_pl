:- use_module(library(real)).

% R interface
:- instead_of <- (
       function(instead, of) :-
           return(instead)).

:- omit_right <- (
    function(expr) :-
        as.list(expr)[[2]]).

:- omit_left <- (
    function(expr) :-
        as.list(expr)[[3]]).

:- frac <- '`/`'.

:- dfrac <- '`/`'.

:- overline <- identity.

:- denoting <- (
       function(x, y, desc) :-
           return(y)).

:- format_tratio <- (
       function(x) :-
           s = sprintf(ifelse(abs(x) > 10, "%.1f", "%.2f"), x);
           return(s)).

:- sub <- (
    function(x, s) :-
        return(x)).
