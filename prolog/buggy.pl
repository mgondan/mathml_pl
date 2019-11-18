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
    html(\item(TTEST)),
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
item('TTEST'('T0'-'EOT', 'T0', 'EOT', mu, s, 'S_T0', 'S_EOT', 'N')) :-
    <- 'T0' = 25.6,
    <- 'EOT' = 19.7,
    <- mu = 4,
    <- s = 4.0,
    <- 'S_T0' = 4.9,
    <- 'S_EOT' = 5.2,
    <- 'N' = 24.

item('TTEST'(T0-EOT, T0, EOT, Mu, s, _S_T0, _S_EOT, N)) -->
    { X_T0_EOT <- 'T0'-'EOT', 
      X_mu <- mu,
      X_s <- s,
      X_N <- 'N',
      X_T0 <- 'T0',
      X_s_T0 <- 'S_T0',
      X_EOT <- 'EOT',
      X_s_EOT <- 'S_EOT' },
    html([h1(["Paired ", \mml(t), "-test"]),
        p(["Consider a clinical study on rumination-focused Cognitive ",
           "Behavioral Therapy (rfCBT) with ", \mml(N = X_N), " ",
	       "patients."]),
    table(class("table"),
          [thead(tr([th(\mml(N = X_N)), th(\mml(T0)),
	          th(\mml(EOT)), th(\mml(T0-EOT))])),
           tbody([tr([th("Mean"), td(\mml(X_T0)), td(\mml(X_EOT)),
                      td(\mml(round1(X_T0_EOT)))]),
  		          tr([th("SD"), td(\mml(X_s_T0)), td(\mml(X_s_EOT)),
                      td(\mml(X_s))])])]),
    p(["Does rfCBT lead to a relevant reduction (i.e., more than ",
       \mml(Mu = X_mu), " HDRS units) in mean HDRS scores between ",
       "Baseline (", \mml(T0), ") and End of Treatment ",
       "(", \mml(EOT), ")? Please determine the ", \mml(t), "-ratio."]),
    div(class("panel panel-default"), 
    div(class("panel-body"),
    div(class("form-group"), 
     [label("Response"),
      div(class("input-group"), 
          [input([class("form-control"), id(response), name(response)]),
            div(class("input-group-btn"),
                button([type(submit), id(submit), class("btn btn-default")], "Submit"))])])))]).

intermediate('TTEST'(_, _, _, _, _, _, _, _)).
intermediate(tratio(_, _, _, _)).

expert('TTEST'(X, _T0, _EOT, Mu, S, _S_T0, _S_EOT, N) = 
       tratio(X, Mu, S, N)).

expert(tratio(X, Mu, S, N) = 
       format_tratio(dfrac(X - Mu, S / sqrt(N)))).

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
