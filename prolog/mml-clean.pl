:- module(mathml,
    [ pl2m/2 ]).

:- use_module(library(real)).
:- use_module(library(http/html_write)).

:- discontiguous pl2m/2.
:- discontiguous paren/2.
:- discontiguous precedence/2.
:- discontiguous example/0.

%
% Punctuation
%
pl2m(punct('_'), &(nbsp)).
pl2m(punct(' '), mspace(width(thickmathspace), [])).
pl2m(punct(ldots), mi(&(hellip))).
pl2m(punct(cdots), mi(&(ctdot))).

paren(punct(_), 0).

example :-
    example(punct('_')).

%
% Operator signs
%
pl2m(op(+), mo(+)).
pl2m(op(-), mo(-)).
pl2m(op(*), mo(&(sdot))).
pl2m(op(/), mo(/)).
pl2m(op(=\=), mo(&('#8203'))).
pl2m(op(=), mo(=)).
pl2m(op(<), mo(&(lt))).
pl2m(op(>), mo(&(gt))).
pl2m(op(=<), mo(&(le))).
pl2m(op(>=), mo(&(ge))).
pl2m(op(\=), mo(&(ne))).
pl2m(op(!), mo(!)).
pl2m(op('%'), mo('%')).
pl2m(op(','), mo(&(comma))).
pl2m(op(';'), mo(&('#59'))).
pl2m(op('|'), mo('|')).
pl2m(op(invisible_times), mo(&('#x2062'))).
pl2m(op(->), mo(&(rArr))).
pl2m(op(~>), mo(&(zigrarr))).
pl2m(op(~), mo(~)).

paren(op(_), 0).

example :-
    example(op(invisible_times)).

%
% Greek letters
%
pl2m(greek(alpha), mi(&(alpha))).
pl2m(greek(mu), mi(&(mu))).
pl2m(greek(pi), mi(&(pi))).
pl2m(greek(sigma), mi(&(sigma))).

paren(greek(_), 0).

example :-
    example(greek(alpha)).

%
% Identifiers
%
pl2m(id(X), mi(X)).

paren(id(_), 0).

example :-
    example(id(x)).

%
% Strings (non-italicized)
%
pl2m(string(X), mtext(X)).

paren(string(_), 0).

example :-
    example(string("text")).

%
% Parentheses
%
pl2m(parentheses(A), mrow([mo('('), M, mo(')')])) :-
    pl2m(A, M).

paren(parentheses(_), 1).

pl2m(bracket(A), mrow([mo('['), M, mo(']')])) :-
    pl2m(A, M).

paren(bracket(_), 2).

pl2m(curly(A), mrow([mo('{'), M, mo('}')])) :-
    pl2m(A, M).

paren(curly(_), 3).

pl2m(abs(A), mrow([mo('|'), M, mo('|')])) :-
    pl2m(A, M).

paren(abs(_), 0).

% Auto-determine level of parentheses
pl2m(paren(A), M) :-
    paren(A, 0),
    pl2m(parentheses(A), M).

pl2m(paren(A), M) :-
    paren(A, 1),
    pl2m(bracket(A), M).

pl2m(paren(A), M) :-
    paren(A, 2),
    pl2m(curly(A), M).

pl2m(paren(A), M) :-
    paren(A, Paren),
    Paren > 2,
    pl2m(parentheses(A), M).

paren(paren(A), Paren) :-
    paren(A, P),
    Paren is P + 1.

example :-
    example(paren(paren(paren(abs(greek(alpha)))))).

%
% Lists (e.g., function arguments)
%
% Default separator: comma
pl2m(list(L), M) :-
    pl2m(list(op(','), L), M).

pl2m(list(_, []), '').

pl2m(list(Sep, [H | T]), mrow([HM | TM])) :-
    pl2m(H, HM),
    pl2m(tail(Sep, T), TM).

pl2m(tail(_, []), []).

pl2m(tail(Sep, [H | T]), [SM, HM | TM]) :-
    pl2m(Sep, SM),
    pl2m(H, HM),
    pl2m(tail(Sep, T), TM).

paren(list(L), Paren) :-
    maplist(paren, L, P),
    max_list(P, Paren).

paren(list(_, L), Paren) :-
    paren(list(L), Paren).

%prec(_, list(Sep, _), P) :-
%    current_op(Prec, _, Sep),
%    !, P = list-Prec.
%
%prec(_, list(_, _), list-0).

example :- 
    example(paren(list([id(x), id(y), paren(id(z))]))).

% Linear model
pl2m(linear(Dep, Icpt, Cov, Strata, Main, Other, _Data), M) :-
    append([Icpt, Cov, Strata, Main, Other], Predictors),
    pl2m(operator(op(~), Dep, list(+, Predictors)), M).

% Lists
pl2m(list(+, []), M) :-
    pl2m(0, M).

pl2m(list(+, [A]), M) :-
    pl2m(A, M).

pl2m(list(+, [H1, H2 | T]), M) :-
    pl2m(operator(op(+), H1, list(+, [H2 | T])), M).

precedence(list(_, []), 0).

precedence(list(+, [_ | _]), Prec) :-
    precedence(op(+), yfx, Prec).

% Binary operators
pl2m(operator(Op, Left, Right), mrow([L, F, R])) :-
    pl2m(Op, F),
    precedence(Op, xfx, Prec),
    precedence(Left, LPrec),
    (   LPrec >= Prec
    ->  pl2m(paren(Left), L)
    ;   pl2m(Left, L)),
    precedence(Right, RPrec),
    (   RPrec >= Prec
    ->  pl2m(paren(Right), R)
    ;   pl2m(Right, R)).

pl2m(operator(Op, Left, Right), mrow([L, F, R])) :-
    pl2m(Op, F),
    precedence(Op, yfx, Prec),
    precedence(Left, LPrec),
    (   LPrec > Prec
    ->  pl2m(paren(Left), L)
    ;   pl2m(Left, L)),
    precedence(Right, RPrec),
    (   RPrec >= Prec
    ->  pl2m(paren(Right), R)
    ;   pl2m(Right, R)).

pl2m(operator(Op, Left, Right), mrow([L, F, R])) :-
    pl2m(Op, F),
    precedence(Op, xfy, Prec),
    precedence(Left, LPrec),
    (   LPrec >= Prec
    ->  pl2m(paren(Left), L)
    ;   pl2m(Left, L)),
    precedence(Right, RPrec),
    (   RPrec > Prec
    ->  pl2m(paren(Right), R)
    ;   pl2m(Right, R)).

% Symbols and variables
pl2m(dependent(A), mi(A)).
pl2m(integer(A), mn(A)).
pl2m(intercept(A), M) :-
    pl2m(integer(A), M).
pl2m(stratum(A), mi(A)).
pl2m(covariate(A), mi(A)).
pl2m(predictor(A), mi(A)).
pl2m(main(A), mi(A)).

% Operator precedence
precedence(operator(Op, _L, _R), Prec) :-
    precedence(Op, yfx, Prec).

precedence(operator(Op, _L, _R), Prec) :-
    precedence(Op, xfy, Prec).

precedence(operator(Op, _L, _R), Prec) :-
    precedence(Op, xfx, Prec).

precedence(op(Op), Fix, Prec) :-
    current_op(Prec, Fix, Op).

precedence(dependent(_), 0).
precedence(integer(_), 0).
precedence(intercept(_), 0).
precedence(stratum(_), 0).
precedence(covariate(_), 0).
precedence(predictor(_), 0).
precedence(main(_), 0).

example :-
    pl2m(linear(dependent(y),
                [intercept(1)],
                [covariate(t0)],
                [stratum(sex), stratum(center)],
                [main(therapy)],
                [],
                data(d)), M),
    writeln(M).

example(P) :-
    writeln(P),
    pl2m(P, M),
    html(M, Out, []),
    print_html(Out).
