:- use_module(library(real)).
:- use_module(library(mathml)).
:- use_module(library(http/html_write)).

:- include('real-0.0.8.pl').
% :- include('mathml-0.0.8.pl').
% :- include('rserve-0.0.8.pl').
:- discontiguous buggy/2.
:- discontiguous intermediate/1.
    
% Calling interface
route(A, B, Path) :-
    route(A, B, [A], Path).

ex :-
    item(TTEST),
    route(TTEST, Sol, Path),
    Res <- Sol,
    mathml(Sol = Res, S), html(html(S)), 
    mathml(list(';', Path), P), html(html(P)).    

% Correct steps in the calculation
step(A, X, [expert(A = X)]) :-
    expert(A = X).

% Apply buggy rule
step(A, X, [buggy(A \= X, Bug)]) :-
    buggy(A \= X, Bug).

% Enter terms
step(A, X, Steps) :-
    compound(A),
    compound_name_arity(A, Name, Arity),
    compound_name_arity(C, Name, Arity),
    enter(C),
    !, compound_name_arguments(A, Name, Args),
    compound_name_arguments(C, Name, Plus),
    maplist(route_check, Plus, Args, New, Sol),
    Args \= New,
    compound_name_arguments(X, Name, New),
    append(Sol, Steps).

step(A, X, Steps) :-
    compound(A),
    compound_name_arguments(A, Name, Args),
    maplist(route, Args, New, Sol),
    Args \= New,
    compound_name_arguments(X, Name, New),
    append(Sol, Steps).

% Do not enter "correct" branch of mistakes 
enter(instead_of(+, -)).
enter(denoting(-, +, -)).
enter(list(-, +)).
enter(color(-, +)).

route_check(-, A, A, []).
route_check(+, A, B, Path) :-
    route(A, B, Path).

% Arrived at goal
route(A, A, _, []) :-
    not(intermediate(A)).

% Depth search
route(A, X, Visited, Solution) :-
    step(A, B, Steps),
    not(member(B, [A | Visited])),
    route(B, X, [A | Visited], S),
    append(Steps, S, Solution).

% Intermediate results
intermediate_check((+)-A) :-
    intermediate(A).

intermediate(A) :-
    compound(A),
    compound_name_arity(A, Name, Arity),
    compound_name_arity(C, Name, Arity),
    enter(C),
    !, compound_name_arguments(A, Name, Args),
    compound_name_arguments(C, Name, Plus),
    pairs_keys_values(Pairs, Plus, Args),
    include(intermediate_check, Pairs, [_ | _]).

intermediate(A) :-
    compound(A),
    compound_name_arguments(A, _Name, Args),
    include(intermediate, Args, [_ | _]).

% Example: Paired t-test
item('TTEST'('T0' - 'EOT', 'T0', 'EOT', mu, s, 
             'S_T0', 'S_EOT', 'N')) :-
    'T0_EOT' <- 5.9,
    mu <- 4,
    s <- 4.0,
    'N' <- as.integer(24),
    'T0' <- 25.6,
    s_T0 <- 4.9,
    'EOT' <- 19.7,
    s_EOT <- 5.2.

%:- <- {|r||
%       T0 = 25.6 ; EOT = 19.7 ; mu = 4 ; s = 4.0 ;
%       S_T0 = 4.9 ; S_EOT = 5.2 ; N = 24
%       |}.

intermediate('TTEST'(_, _, _, _, _, _, _, _)).
intermediate(tratio(_, _, _, _)).

expert('TTEST'(X, _T0, _EOT, Mu, S, _S_T0, _S_EOT, N) = 
       tratio(X, Mu, S, N)).

expert(tratio(X, Mu, S, N) = 
       dfrac(X - Mu, S / sqrt(N))).

% Wrong X
buggy(TTEST \= TRATIO, BUG) :-
    TTEST = 'TTEST'(D, T0, _EOT, Mu, S, _S_T0, _S_EOT, N),
    TRATIO = tratio(W, Mu, S, N),
    BUG = bug(data, instead_of('T0', 'T0'-'EOT')),
    W = denoting(overline('D'), instead_of(T0, D), 
        "the average change between baseline and end of treatment").

% Omit implicit parenthesis
buggy(dfrac(A - B, C / D) \= A - dfrac(B, C) / D, Bug) :-
    Bug = bug(calc, dfrac(red(paren(black('A' - 'B'))), 
                         red(paren(black('C' / 'D'))))).
