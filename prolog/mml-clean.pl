:- module(mathml,
    [ pl2m/2 ]).

:- use_module(library(real)).
:- use_module(library(http/html_write)).

:- discontiguous pl2m/3.
:- discontiguous paren/3.
:- discontiguous precedence/4.
:- discontiguous example/0.

%
% Punctuation
%
pl2m(_, punct('_'), &(nbsp)).
pl2m(_, punct(' '), mspace(width(thickmathspace), [])).
pl2m(_, punct(ldots), mi(&(hellip))).
pl2m(_, punct(cdots), mi(&(ctdot))).

paren(_, punct(_), 0).

precedence(_, punct(_), punct, 0).

example :-
    example(punct('_')).

%
% Operator signs
%
pl2m(_, op(+), mo(+)).
pl2m(_, op(-), mo(-)).
pl2m(_, op(*), mo(&(sdot))).
pl2m(_, op(/), mo(/)).
pl2m(_, op(=\=), mo(&('#8203'))).
pl2m(_, op(=), mo(=)).
pl2m(_, op(<), mo(&(lt))).
pl2m(_, op(>), mo(&(gt))).
pl2m(_, op(=<), mo(&(le))).
pl2m(_, op(>=), mo(&(ge))).
pl2m(_, op(\=), mo(&(ne))).
pl2m(_, op(!), mo(!)).
pl2m(_, op('%'), mo('%')).
pl2m(_, op(','), mo(&(comma))).
pl2m(_, op(';'), mo(&('#59'))).
pl2m(_, op('|'), mo('|')).
pl2m(_, op(invisible_times), mo(&('#x2062'))).
pl2m(_, op(->), mo(&(rArr))).
pl2m(_, op(~>), mo(&(zigrarr))).
pl2m(_, op(~), mo(~)).
pl2m(_, op(''), '').

paren(_, op(_), 0).

precedence(_, op(Op), op, Precedence) :-
    current_op(P, Fix, Op),
    Precedence = P.

example :-
    example(op(invisible_times)).

%
% Greek letters
%
pl2m(_, greek(alpha), mi(&(alpha))).
pl2m(_, greek(mu), mi(&(mu))).
pl2m(_, greek(pi), mi(&(pi))).
pl2m(_, greek(sigma), mi(&(sigma))).

paren(_, greek(_), 0).

precedence(_, greek(_), greek, 0).

example :-
    example(greek(alpha)).

%
% Identifiers
%
pl2m(_, id(X), mi(X)).

paren(_, id(_), 0).

precedence(_, id(_), id, 0).

example :-
    example(id(x)).

%
% Strings (non-italicized)
%
pl2m(_, string(X), mtext(X)).

paren(_, string(_), 0).

precedence(_, string(_), string, 0).

example :-
    example(string("text")).

%
% Parentheses
%
pl2m(Flags, parentheses(A), mrow([mo('('), M, mo(')')])) :-
    pl2m(Flags, A, M).

paren(_, parentheses(_), '(').

precedence(_, parentheses(_), paren, 0).

pl2m(Flags, bracket(A), mrow([mo('['), M, mo(']')])) :-
    pl2m(Flags, A, M).

paren(_, bracket(_), '[').

precedence(_, bracket(_), paren, 0).

pl2m(Flags, curly(A), mrow([mo('{'), M, mo('}')])) :-
    pl2m(Flags, A, M).

paren(_, curly(_), '{').

precedence(_, curly(_), paren, 0).

pl2m(Flags, abs(A), mrow([mo('|'), M, mo('|')])) :-
    pl2m(Flags, A, M).

paren(_, abs(_), 0).

precedence(_, abs(_), paren, 0).

% Auto-determine level of parentheses
pl2m(Flags, paren(A), M) :-
    paren(Flags, A, 0),
    pl2m(Flags, parentheses(A), M).

pl2m(Flags, paren(A), M) :-
    paren(Flags, A, '('),
    pl2m(Flags, bracket(A), M).

pl2m(Flags, paren(A), M) :-
    paren(Flags, A, '['),
    pl2m(Flags, curly(A), M).

pl2m(Flags, paren(A), M) :-
    paren(Flags, A, '{'),
    pl2m(Flags, parentheses(A), M).

paren(Flags, paren(A), '(') :-
    paren(Flags, A, 0).

paren(Flags, paren(A), '[') :-
    paren(Flags, A, '(').

paren(Flags, paren(A), '{') :-
    paren(Flags, A, '[').

paren(Flags, paren(A), '{') :-
    paren(Flags, A, '{').

precedence(_, paren(_), paren, 0).

example :-
    example(paren(paren(paren(abs(greek(alpha)))))).

%
% Lists (e.g., function arguments)
%
% Default separator: from Flags
pl2m(Flags, list(L), M) :-
    member(listsep(Sep), Flags),
    pl2m(Flags, list(Sep, L), M).

% Default separator: comma
pl2m(Flags, list(L), M) :-
    pl2m(Flags, list(op(','), L), M).

pl2m(_, list(_, []), '').

pl2m(Flags, list(Sep, [H | T]), mrow([HM | TM])) :-
    pl2m(Flags, H, HM),
    pl2m(Flags, tail(Sep, T), TM).

pl2m(_, tail(_, []), []).

pl2m(Flags, tail(Sep, [H | T]), [SM, HM | TM]) :-
    pl2m(Flags, Sep, SM),
    pl2m(Flags, H, HM),
    pl2m(Flags, tail(Sep, T), TM).

paren(Flags, list(L), Paren) :-
    maplist(paren(Flags), L, P),
    max_list(P, Paren).

paren(Flags, list(_, L), Paren) :-
    paren(Flags, list(L), Paren).

precedence(Flags, list(L), Op, Prec) :-
    member(listsep(Sep), Flags),
    precedence(Flags, list(Sep, L), Op, Prec).

precedence(Flags, list(Sep, _), Op, Prec) :-
    precedence(Flags, Sep, Op, Prec).

example :- 
    example(paren(list([id(x), id(y), paren(id(z))]))).

%
% Colors
%
color(0, "black").
color(1, "red").
color(2, "blue").
color(3, "#00BF00").
color(4, "#9F5F00").
color(5, "#7F007F").
color(6, "#007F7F").

pl2m(Flags, color(num(C), A), mstyle(mathcolor(Col), M)) :-
   color(C, Col),
   pl2m(Flags, A, M).

pl2m(Flags, color(name(C), A), mstyle(mathcolor(C), M)) :-
   pl2m(Flags, A, M).

paren(Flags, color(_, A), Paren) :-
    paren(Flags, A, Paren).

pl2m(Flags, black(A), M) :-
   pl2m(Flags, color(name("black"), A), M).

precedence(Flags, color(_, A), Op, Prec) :-
    precedence(Flags, A, Op, Prec).

precedence(Flags, black(A), Op, Prec) :-
    precedence(Flags, A, Op, Prec).

example :-
    example(color(name("red"), paren(black(id(x))))).

%
% Decorations
%
pl2m(Flags, overline(A), mover(accent(true), [M, mo(&(macr))])) :-
    pl2m(Flags, A, M).

paren(Flags, overline(A), Paren) :-
    paren(Flags, A, Paren).

% Put average(x)^2 in parentheses
precedence(Flags, overline(A), Op, Prec) :-
    precedence(Flags, op(+), Op, Prec).

% Underbrace with text
pl2m(Flags, underbrace(A, Under), munder([munder(accentunder(true), [M, mo(stretchy(true), &('UnderBrace'))]), MU])) :-
    pl2m(Flags, A, M),
    pl2m(Flags, Under, MU).

paren(Flags, underbrace(A, _), Paren) :-
    paren(Flags, A, Paren).

precedence(Flags, underbrace(A, _), Op, Prec) :-
    precedence(Flags, A, Op, Paren).

% Strike through
pl2m(strike(Color, A), M) :-
   pl2m(color(Color, strike(black(A))), M).

paren(strike(_, A), Paren) :-
    paren(A, Paren).

pl2m(strike(A), menclose(notation(updiagonalstrike), M)) :-
    pl2m(A, M).

paren(strike(A), Paren) :-
    paren(A, Paren).

% Rounded box
pl2m(roundedbox(A), menclose(notation(roundedbox), M)) :-
    pl2m(A, M).

paren(roundedbox(A), Paren) :-
    paren(A, Paren).

pl2m(roundedbox(Color, A), M) :-
    pl2m(color(Color, roundedbox(black(A))), M).

paren(roundedbox(_, A), Paren) :-
    paren(A, Paren).

% invisible
pl2m(phantom(A), mphantom(M)) :-
    pl2m(A, M).

paren(phantom(A), Paren) :-
    paren(A, Paren).

% Superscript and subscript
pl2m(subsup(A, B, C), msubsup([X, Y, Z])) :-
    precedence(subsup(A, B, C), Prec),
    precedence(A, P),
    ( Prec =< P
      -> pl2m(paren(A), X)
      ; pl2m(A, X)
    ), pl2m(B, Y),
    pl2m(C, Z).

paren(subsup(A, _, _), Paren) :-
    precedence(subsup(A, B, C), Prec),
    precedence(A, P),
    ( Prec =< P
      -> paren(paren(A), Paren)
      ; paren(A, Paren)
    ).

example :- 
    example(strike(num(1), id(x))).
    
example :- 
    example(underbrace(id(s), list(op(''), [string("instead of"), punct(' '), greek(sigma)]))).

% Linear model
pl2m(linear(Dep, Icpt, Cov, Strata, Main, Other, _Data), M) :-
    append([Icpt, Cov, Strata, Main, Other], Predictors),
    pl2m(operator(op(~), Dep, list(+, Predictors)), M).

%
% Operators
%
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

%
% Operators
%
ml(Flags, sub(A, B), M) :-
    select_option(replace(sub(A, B), subsup(A, B, C)), Flags, New),
    !, ml(New, subsup(A, B, C), M).

paren(Flags, sub(A, B), P) :-
    select_option(replace(sub(A, B), subsup(A, B, C)), Flags, New),
    !, paren(New, subsup(A, B, C), P).

prec(Flags, sub(A, B), P) :-
    select_option(replace(sub(A, B), subsup(A, B, C)), Flags, New),
    !, precedence(New, subsup(A, B, C), P).

ml(Flags, sub(A, B), msub([X, Y])) :-
    precedence(Flags, sub(A, B), _-P),
    precedence(Flags, A, _-Inner),
    ( P < Inner
      -> ml(Flags, paren(A), X)
      ; ml(Flags, A, X)
    ), ml(Flags, B, Y).

paren(Flags, sub(A, _), P) :-
    paren(Flags, A, P).

prec(Flags, sub(A, _), sub-P) :-
    precedence(Flags, A, _-P).

math(Flags, A^B, Flags, sup(A, B)).

% experimental: sin^2 x for "simple" x
ml(Flags, sup(Sin, X), M) :-
    precedence(Flags, Sin, trig-_),
    paren(Flags, X, 0),
    precedence(Flags, X, _-0),
    !, ml([replace(Sin^X, Sin)], Sin, M).

ml(Flags, sup(A, B), M) :-
    select_option(replace(sup(A, B), subsup(A, C, B)), Flags, New),
    !, ml(New, subsup(A, C, B), M).

paren(Flags, sup(A, B), P) :-
    select_option(replace(sup(A, B), subsup(A, C, B)), Flags, New),
    !, paren(New, subsup(A, C, B), P).

prec(Flags, sup(A, B), P) :-
    select_option(replace(sup(A, B), subsup(A, C, B)), Flags, New),
    !, precedence(New, subsup(A, C, B), P).

ml(Flags, sup(A, B), msup([X, Y])) :-
    precedence(Flags, sup(A, B), _-P),
    precedence(Flags, A, _-Inner),
    ( P =< Inner
      -> ml(Flags, paren(A), X)
      ; ml(Flags, A, X)
    ), ml(Flags, B, Y).

paren(Flags, sup(A, _), P) :-
    paren(Flags, A, P).

prec(_, sup(_, _), sup-P) :-
    current_op(Prec, xfy, ^),
    P = Prec.

% Omit multiplication sign in "simple" products
math(Flags, A * B, Flags, M) :-
    paren(Flags, A / x, 0),
    !, M = A invisible_times B.

% Use plus as default separator for lists right to ~
math(Flags, Dependent ~ Predictors, [sep-(+) | Flags], operator(Prec, xfy, ~, Dependent, Predictors)) :-
    current_op(P, xfy, ','),
    precedence(Flags, Predictors, list-P),
    current_op(Prec, xfy, ~).

% Negative sign has same precedence as binary minus
math(Flags, -A, Flags, operator(P, fx, -, A)) :-
    precedence(Flags, a-b, _-P).

% Prefix and postfix operators (e.g., factorial)
math(Flags, Comp, Flags, operator(Prec, Fix, Op, A)) :-
    compound(Comp),
    compound_name_arguments(Comp, Op, [A]),
    current_op(P, Fix, Op), Prec = P,
    member(Fix, [xf, yf, fx, fy]).

ml(Flags, operator(P, fx, Op, A), mrow([F, X])) :-
    precedence(Flags, A, _-Inner),
    ( P =< Inner
      -> ml(Flags, paren(A), X)
      ; ml(Flags, A, X)
    ), ml(Flags, Op, F).

ml(Flags, operator(P, fy, Op, A), mrow([F, Y])) :-
    precedence(Flags, A, _-Inner),
    ( P < Inner
      -> ml(Flags, paren(A), Y)
      ; ml(Flags, A, Y)
    ), ml(Flags, Op, F).

ml(Flags, operator(P, xf, Op, A), mrow([X, F])) :-
    precedence(Flags, A, _-Inner),
    ( P =< Inner
      -> ml(Flags, paren(A), X)
      ; ml(Flags, A, X)
    ), ml(Flags, Op, F).

ml(Flags, operator(P, yf, Op, A), mrow([Y, F])) :-
    precedence(Flags, A, _-Inner),
    ( P < Inner
      -> ml(Flags, paren(A), Y)
      ; ml(Flags, A, Y)
    ), ml(Flags, Op, F).

paren(Flags, operator(Prec, Fix, _, A), Paren) :-
    member(Fix, [xf, fx]),
    paren(Flags, A, P),
    precedence(Flags, A, _-Inner),
    ( Prec =< Inner
      -> Paren is P + 1
      ; Paren = P
    ).

paren(Flags, operator(Prec, Fix, _, A), Paren) :-
    member(Fix, [yf, fy]),
    paren(Flags, A, P),
    precedence(Flags, A, _-Inner),
    ( Prec < Inner
      -> Paren is P + 1
      ; Paren = P
    ).

prec(_, operator(P, _, _, _), op-P).

% Avoid unnecessary parentheses right to + in 1 + (2 - 3)
math(Flags, A + B, Flags, operator(P, yfy, +, A, B)) :-
    current_op(P, yfx, +).

% General binary operators
math(Flags, Comp, Flags, operator(Prec, Fix, Op, A, B)) :-
    compound(Comp),
    compound_name_arguments(Comp, Op, [A, B]),
    current_op(P, Fix, Op), Prec = P,
    member(Fix, [xfx, yfx, xfy]).

ml(Flags, operator(P, xfx, Op, A, B), mrow([X, F, Y])) :-
    precedence(Flags, A, _-PrecA),
    ( P =< PrecA
      -> ml(Flags, paren(A), X)
      ; ml(Flags, A, X)
    ), precedence(Flags, B, _-PrecB),
    ( P =< PrecB
      -> ml(Flags, paren(B), Y)
      ; ml(Flags, B, Y)
    ), ml(Flags, Op, F).

ml(Flags, operator(P, xfy, Op, A, B), mrow([X, F, Y])) :-
    precedence(Flags, A, _-PrecA),
    ( P =< PrecA
      -> ml(Flags, paren(A), X)
      ; ml(Flags, A, X)
    ), precedence(Flags, B, _-PrecB),
    ( P < PrecB
      -> ml(Flags, paren(B), Y)
      ; ml(Flags, B, Y)
    ), ml(Flags, Op, F).

% yfy avoids parentheses around 1 + (2 + 3)
ml(Flags, operator(P, yfy, Op, A, B), mrow([Y, F, X])) :-
    precedence(Flags, A, _-PrecA),
    ( P < PrecA
      -> ml(Flags, paren(A), Y)
      ; ml(Flags, A, Y)
    ), precedence(Flags, B, _-PrecB),
    ( P < PrecB
      -> ml(Flags, paren(B), X)
      ; ml(Flags, B, X)
    ), ml(Flags, Op, F).

ml(Flags, operator(P, yfx, Op, A, B), mrow([Y, F, X])) :-
    precedence(Flags, A, _-PrecA),
    ( P < PrecA
      -> ml(Flags, paren(A), Y)
      ; ml(Flags, A, Y)
    ), precedence(Flags, B, _-PrecB),
    ( P =< PrecB
      -> ml(Flags, paren(B), X)
      ; ml(Flags, B, X)
    ), ml(Flags, Op, F).

paren(Flags, operator(Prec, xfx, _, A, B), P) :-
    paren(Flags, A, PA),
    precedence(Flags, A, _-PrecA),
    ( Prec =< PrecA
      -> ParenA is PA + 1
      ; ParenA = PA
    ), paren(Flags, B, PB),
    precedence(Flags, B, _-PrecB),
    ( Prec =< PrecB
      -> ParenB is PB + 1
      ; ParenB = PB
    ), P is max(ParenA, ParenB).

paren(Flags, operator(Prec, yfx, _, A, B), P) :-
    paren(Flags, A, PA),
    precedence(Flags, A, _-PrecA),
    ( Prec < PrecA
      -> ParenA is PA + 1
      ; ParenA = PA
    ), paren(Flags, B, PB),
    precedence(Flags, B, _-PrecB),
    ( Prec =< PrecB
      -> ParenB is PB + 1
      ; ParenB = PB
    ), P is max(ParenA, ParenB).

paren(Flags, operator(Prec, xfy, _, A, B), P) :-
    paren(Flags, A, PA),
    precedence(Flags, A, _-PrecA),
    ( Prec =< PrecA
      -> ParenA is PA + 1
      ; ParenA = PA
    ), paren(Flags, B, PB),
    precedence(Flags, B, _-PrecB),
    ( Prec < PrecB
      -> ParenB is PB + 1
      ; ParenB = PB
    ), P is max(ParenA, ParenB).

prec(_, operator(P, _, _, _, _), op-P).

example :- example(a^3 + 3*a^2*b + 3*a*b^2 + b^3).
example :- example(a^b).
example :- example((s!)!).
example :- example(a + b + c).
example :- example(a + (b + c)).
example :- example(a - b - c).
example :- example(a - (b - c)).
example :- example((a + b) * (a - b) = a^two - b^two).









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
