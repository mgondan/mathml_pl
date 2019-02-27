:- module(mathml, [
    mathml/2,
    op(180, fy, sin),
    op(180, fy, cos),
    op(180, fy, tan),
    op(180, xf, factorial)]).

:- discontiguous ml/2.
:- discontiguous math/2.

% This collection of predicates is used to translate
% mathematical expressions to MathML ("presentation
% markup"), for example, sqrt(2) to math(sqrt(mn(2))). The
% output can then be used to render mathematical expressions
% using SWISH's html//1 interface which produces something
% like <SQRT> <MN> 2 </MN> </SQRT>.
%
% This is work in progress by a Prolog beginner. Hints,
% wishes, and other feedback to Matthias.Gondan@psy.ku.dk
%
% Conversion is done in two steps:
%
% * math(+X, ?Y) translates the mathematical expression to a
%   "standardized" math representation. This basically
%   replaces the missing support for "content math" in most
%   browsers. For example, we have
%   math((-1) * (-1), Y)), with
%     Y = parenthesis(-1)*parenthesis(-1). math/2 also
%   deals with nested parentheses, for example,
%   math(paren(paren(1)), bracket(paren(1))).
%
% * ml/2 then converts the result to compounds that can
%   be dealt with by swish:html//1.
%
% * Support for MathJax output could easily be added by
%   a function jax/2 that replaces ml/2, thereby adding
%   support for a few more browsers.
%
mathml(X, math(Y)) :-
    math(X, Z),
    ml(Z, Y).

mathml(X) :-
    mathml(X, Atoms, []),
    html(Atoms).

mathml(X) -->
    {mathml(X, Y)},
    html(Y).

% Omit left part of an operation
math(omit_left(L, Op, R), New) :-
    C =.. [Op, L, ''],
    !, math(nospace(cancel(C), R), New).

% Omit right part of an operation
math(omit_right(L, Op, R), New) :-
    C =.. [Op, '', R],
    !, math(nospace(L, cancel(C)), New).

% Erroneously add something to the left
math(invent_left(L, Op, R), New) :-
    C =.. [Op, red(uwave(black(hideerror(L)))), R],
    !, math(C, New).

% Erroneously add something to the right
math(invent_right(L, Op, R), New) :-
    C =.. [Op, L, red(uwave(black(hideerror(R))))],
    !, math(C, New).

% A instead of B
math(instead_of(A, B), New) :-
    !, math(underbrace(A, space("instead of", B)), New).

math(instead_of(A, AA, B), New) :-
    !, math(underbrace(A, space(AA, "instead of", B)), New).

% Binomial density and distribution
math(dbinom(K, N, P), New) :-
    !, math(function(sub('P', "Bi"), paren('X' = K | (N, P))), New).

math(pbinom(K, N, P), New) :-
    !, math(function(sub('P', "Bi"), paren('X' =< K | (N, P))), New).

math(ubinom(K, N, P), New) :-
    !, math(function(sub('P', "Bi"), paren('X' >= K | (N, P))), New).

math(uqbinom(Alpha, N, P), New) :-
    !, math(function(sub('Q', "Bi"), paren(1-Alpha | (N, P))), New).

math(qbinom(Alpha, N, P), New) :-
    !, math(function(sub('Q', "Bi"), paren(Alpha | (N, P))), New).

math(bernoulli(K, N, P), New) :-
    !, math(successes(K, P) * failures(N - K, P), New).

math(successes(K, P), New) :-
    !, math(P^K, New).

math(failures(K, P), New) :-
    !, math((1 - P)^K, New).

% Treat negative numbers as compounds (insert parentheses if needed)
math(X, New) :-
    number(X),
    X < 0,
    !, A is abs(X),
    math(-A, New).

% Check if parentheses are needed between operators
math(C, New) :-
    compound(C),
    compound_name_arguments(C, Op, [Arg]),
    current_op(P, Fix, Op),
    member(Fix, [xf, yf]),
    !, left(P, Arg, NewArg),
    compound_name_arguments(New, Op, [NewArg]).

math(C, New) :-
    compound(C),
    compound_name_arguments(C, Op, [Arg]),
    current_op(P, Fix, Op),
    member(Fix, [fx, fy]),
    !, right(Op, P, Arg, NewArg),
    compound_name_arguments(New, Op, [NewArg]).

math(C, New) :-
    compound(C),
    compound_name_arguments(C, Op, [A, B]),
    current_op(P, Fix, Op),
    member(Fix, [xfx, yfx, xfy]),
    !, left(P, A, NewA),
    right(Op, P, B, NewB),
    compound_name_arguments(New, Op, [NewA, NewB]).

% Check if parenthesis is needed (left to an operator)
left(Prec, Arg, New) :-
    prec(Arg, Inner),
    Inner > Prec,
    !, math(paren(Arg), New).

left(_, Arg, New) :-
    math(Arg, New).

% Check if parenthesis is needed (right to an operator)
right(^, Prec, Arg, New) :-
    current_op(Prec, xfy, ^),
    !, math(brace(Arg), New).

right(_, Prec, Arg, New) :-
    prec(Arg, Inner),
    Inner > Prec,
    !, math(paren(Arg), New).

right(Op, Prec, Arg, New) :-
    member(Op, [-, /, sin, cos, tan]),
    prec(Arg, Prec),
    !, math(paren(Arg), New).

right(_, _, Arg, New) :-
    math(Arg, New).

% Determine operator precedence in nested expressions
prec(X, Prec) :-
    number(X),
    X < 0,
    !, current_op(Prec, yfx, -).

prec(X, 0) :-
    atomic(X), !.

% Wrapper for formatting functions
prec(red(X), P) :-
    !, prec(X, P).

prec(green(X), P) :-
    !, prec(X, P).

prec(blue(X), P) :-
    !, prec(X, P).

prec(quote(X), P) :-
    !, prec(X, P).

prec(hideerror(X), P) :-
    !, prec(X, P).

prec(black(X), P) :-
    !, prec(X, P).

prec(color(_, X), P) :-
    !, prec(X, P).

prec(cancel(X), P) :-
    !, prec(X, P).

prec(overline(X), P) :-
    !, prec(X, P).

prec(underbrace(X, _), P) :-
    !, prec(X, P).

prec(uwave(X), P) :-
    !, prec(X, P).

prec(sub(X, _), P) :-
    !, prec(X, P).

prec(subsup(X, _, _), P) :-
    !, prec(X, P).

prec(underbrace(X, _), P) :-
    !, prec(X, P).

prec(argmin(_, _, _, F), P) :-
    !, prec(F, P).

prec(argmax(_, _, _, F), P) :-
    !, prec(F, P).

prec('Sum'(_, _, _, X), P) :-
    !, prec(X, P).

% Operators
prec(X, P) :-
    compound(X),
    compound_name_arity(X, Name, 1),
    current_op(Prec, Fix, Name),
    member(Fix, [fx, fy, yf]),
    !, P = Prec.

prec(X, P) :-
    compound(X),
    compound_name_arity(X, Name, 2),
    current_op(Prec, Fix, Name),
    member(Fix, [yfx, xfx, xfy]),
    !, P = Prec.

% Default, e.g., sqrt(x)
prec(X, 0) :-
    compound(X).

% Nested parentheses
% * paren(red(paren(a + b))) --> bracket(red(paren(a + b)))
% * paren(frac(1, 2 * paren(a+b))) --> paren(frac(1, 2 * paren(a+b)))
math(paren(X), New) :-
    math(X, Z),
    paren(Z, 0),
    !, math(parenthesis(X), New).

math(paren(X), New) :-
    math(X, Z),
    paren(Z, 1),
    !, math(bracket(X), New).

math(paren(X), New) :-
    !, math(curly(X), New).

% Default handlers
math(X, New) :-
    atomic(X),
    !, New = X.

math(X, New) :-
    compound(X),
    !, compound_name_arguments(X, Name, Args),
    maplist(math, Args, NewArgs),
    compound_name_arguments(New, Name, NewArgs).

% Determine depth of parentheses
paren(X, 0) :-
    atomic(X),
    !.

paren(frac(_, _), 0) :-
    !.

paren(dfrac(_, _), 0) :-
    !.

paren(sqrt(_, _), 0) :-
    !.

paren(choose(_, _), 1) :-
    !.

paren(sub(X, _), P) :-
    !, paren(X, P).

paren(sup(X, _), P) :-
    !, paren(X, P).

paren(X^_, P) :-
    !, paren(X, P).

paren(subsup(X, _, _), P) :-
    !, paren(X, P).

paren(underbrace(X, _), P) :-
    !, paren(X, P).

paren('Sum'(_, _, _, X), P) :-
    !, paren(X, P).

paren(argmin(_, _, _, X), P) :-
    !, paren(X, P).

paren(argmax(_, _, _, X), P) :-
    !, paren(X, P).

paren(parenthesis(_), P) :-
    !, P = 1.

paren(bracket(_), P) :-
    !, P = 2.

paren(curly(_), P) :-
    !, P = 3.

paren(paren(X), P) :-
    !, paren(X, P1),
    P is P1 + 1.

paren(X, P) :-
    compound(X),
    compound_name_arguments(X, _Name, Args),
    paren_list(Args, P).

paren_list([], 0).

paren_list([H | T], P) :-
    paren(H, PH),
    paren_list(T, PT),
    P is max(PH, PT).

% Translate math expressions to mathml

% Operators
ml(X, mo(Mo)) :-
    mo(X, O),
    !, Mo = O.

mo(cdots, \['&ctdot;']).
mo(',', \[',']).
mo(';', \['&#59;']).
mo('|', \['|']).
mo(+, +).
mo(-, -).
mo(*, \['&sdot;']).
mo('.', \['&sdot;']).
mo(/, /).
mo(' ', \['&nbsp;']).
mo(=\=, \['&#8203;']).
mo(=, =).
mo(<, \['&lt;']).
mo(>, \['&gt;']).
mo(=<, \['&le;']).
mo(>=, \['&ge;']).
mo(\=, \['&ne;']).
mo(factorial, !).
mo(!, !).

% Identifiers
ml(X, mi(Mi)) :-
    mi(X, I),
    !, Mi = I.

ml(X, mi(Mi)) :-
    atom(X),
    !, Mi = X.

mi(alpha, \['&alpha;']).
mi(mu, \['&mu;']).
mi(pi, \['&pi;']).
mi(sigma, \['&sigma;']).
mi(sin, sin).
mi(cos, cos).
mi(tan, tan).

% Numbers
ml(X, mn(Mn)) :-
    mn(X, N),
    !, Mn = N.

mn('#'(X, 0), N) :-
    number(X),
    format(string(Mn), "~0f", [X]),
    !, N = Mn.

mn('#'(X, 1), N) :-
    number(X),
    format(string(Mn), "~1f", [X]),
    !, N = Mn.

mn('#'(X, 2), N) :-
    number(X),
    format(string(Mn), "~2f", [X]),
    !, N = Mn.

mn('#'(X, 3), N) :-
    number(X),
    format(string(Mn), "~3f", [X]),
    !, N = Mn.

mn('%'(X), N) :-
    number(X),
    Perc is X*100,
    format(string(Mn), "~0f%", [Perc]),
    !, N = Mn.

mn(X, N) :-
    number(X),
    !, N = X.

% String
ml(X, mtext(A)) :-
    string(X),
    !, atom_string(A, X).

% Expressions in parentheses
ml(brace(X), mrow(Y)) :-
    ml(X, Y).

ml(parenthesis(X), mfenced([separators('')], Y)) :-
    ml(X, Y).

ml(bracket(X), mfenced([open('['), close(']'), separators('')], Y)) :-
    ml(X, Y).

ml(curly(X), mfenced([open('{'), close('}'), separators('')], Y)) :-
    ml(X, Y).

% Abbreviations
ml(with(X, _Term, _Desc), Y) :-
    ml(X, Y).

% Subscripts, Power
ml(sub(X, Sub), msub([Y, New])) :-
    ml(brace(X), Y),
    ml(brace(Sub), New).

ml(X^Pow, New) :-
    !, ml(sup(X, Pow), New).

ml(sup(X, Sup), msup([Y, New])) :-
    !, ml(X, Y),
    ml(Sup, New).

ml(subsup(X, Sub, Sup), msubsup([Y, NewSub, NewSup])) :-
    ml(brace(X), Y),
    ml(brace(Sub), NewSub),
    ml(brace(Sup), NewSup).

ml(argmin(C, _, _, F), mrow([munder([argmin, NewC]), \['&nbsp;'], NewF])) :-
    ml(C, NewC),
    ml(F, NewF).

ml(argmax(C, _, _, F), mrow([munder([argmax, NewC]), \['&nbsp;'], NewF])) :-
    ml(C, NewC),
    ml(F, NewF).

ml('Sum'(I, From, To, Exp), mrow([munderover([\['&sum;'], NewF, NewT]), NewExp])) :-
    ml(brace(I=From), NewF),
    ml(To, NewT),
    ml(Exp, NewExp).

% Proper fractions
ml(frac(A, B), mfrac([NewA, NewB])) :-
    ml(brace(A), NewA),
    ml(brace(B), NewB).

ml(dfrac(A, B), New) :-
    ml(frac(A, B), New).

% Functions
ml(function(F, X), mrow([NewF, mo(\['&ApplyFunction;']), NewX])) :-
    ml(F, NewF),
    ml(X, NewX).

ml(sqrt(X), msqrt(Y)) :-
    ml(X, Y).

:- op(180, xf, factorial).

:- op(180, fy, sin).
%ml(sin(X), New) :-
%    ml(function(sin, X), New).

:- op(180, fy, cos).
%ml(cos(X), New) :-
%    ml(function(cos, X), New).

:- op(180, fy, tan).
%ml(tan(X), New) :-
%    ml(function(tan, X), New).

ml(choose(N, K), mfenced(mfrac([linethickness(0)], [NewN, NewK]))) :-
    ml(N, NewN),
    ml(K, NewK).

% Decorations
ml(overline(X), mover([accent(true)], [Y, mo(\['&macr;'])])) :-
    ml(X, Y).

ml(underbrace(X, Under), munder([munder([accentunder(true)],
        [Y, mo([stretchy(true)], \['&UnderBrace;'])]), New])) :-
    ml(X, Y),
    ml(Under, New).

ml(uwave(X), munder([accentunder(true)], [Y, mo([stretchy(true)], \[~])])) :-
    ml(X, Y).

ml(cancel(X), Y) :-
    ml(red(strike(black(X))), Y).

ml(strike(X), menclose([notation(updiagonalstrike)], Y)) :-
    ml(X, Y).

ml(hideerror(X), Y) :-
    ml(X, Y).

ml(red(X), Y) :-
    ml(color("red", X), Y).

ml(green(X), Y) :-
    ml(color("green", X), Y).

ml(black(X), Y) :-
    ml(color("black", X), Y).

ml(color(Col, X), mstyle([color(Col)], Y)) :-
    ml(X, Y).

ml(bbox(Col, X), mstyle([background(Col)], Y)) :-
    ml(X, Y).

ml(quote(X), Y) :-
    ml(X, Y).

ml(space(A, B), mfenced([open(''), close(''), separators('')], [NewA, Space, NewB])) :-
    !, ml(' ', Space),
    ml(A, NewA),
    ml(B, NewB).

ml(space(A, B, C), mfenced([open(''), close(''), separators('')], [NewA, Space, NewB, Space, NewC])) :-
    !, ml(' ', Space),
    ml(A, NewA),
    ml(B, NewB),
    ml(C, NewC).

ml(nospace(A, B), mfenced([open(''), close(''), separators('')], [NewA, NewB])) :-
    !, ml(A, NewA),
    ml(B, NewB).

ml(nospace(A, B, C), mfenced([open(''), close(''), separators('')], [NewA, NewB, NewC])) :-
    !, ml(A, NewA),
    ml(B, NewB),
    ml(C, NewC).

% Operators
ml(A | B, mfenced([open(''), close(''), separators('|')], [NewA, NewB])) :-
    !, ml(A, NewA),
    ml(B, NewB).

ml(C, mrow([NewArg, NewOp])) :-
    compound(C),
    compound_name_arguments(C, Op, [Arg]),
    current_op(_, Fix, Op),
    member(Fix, [xf, yf]),
    !, ml(Arg, NewArg),
    ml(Op, NewOp).

ml(C, mrow([NewOp, NewArg])) :-
    compound(C),
    compound_name_arguments(C, Op, [Arg]),
    current_op(_, Fix, Op),
    member(Fix, [fx, fy]),
    !, ml(Arg, NewArg),
    ml(Op, NewOp).

ml(C, mrow([NewA, NewOp, NewB])) :-
    compound(C),
    compound_name_arguments(C, Op, [A, B]),
    current_op(_, Fix, Op),
    member(Fix, [xfx, yfx, xfy]),
    !, ml(A, NewA),
    ml(B, NewB),
    ml(Op, NewOp).

% Convert expression to HTML
html(X, S) :-
    html(X, List, []),
    partition(\=(nl(_)), List, Part, _),
    with(X, W),
    list_to_set(W, WS),
    denoting(WS, Den, []),
    append(Part, Den, Atomics),
    atomics_to_string(Atomics, S).

% Collect abbreviations
with(with(Exp, Term, Desc), [with(Exp = Term, Desc) | T]) :-
    !, with(Exp, T).

with(X, []) :-
    atomic(X).

with(X, W) :-
    compound(X),
    compound_name_arguments(X, _, Args),
    maplist(with, Args, List),
    append(List, W).

denoting([]) -->
    html("").

denoting([with(XP, Desc) | T]) -->
    html([", with ", \jax(XP), " denoting ", Desc]),
    denoting_and(T).

denoting_and([]) -->
    html(".").

denoting_and([with(XP, Desc) | T]) -->
    html([", with ", \jax(XP), " denoting ", Desc]),
    denoting_and(T).

% Some examples for testing
example1(X) :-
    writeln([exp]),
    writeln(X),
    math(X, Y),
    writeln([math]),
    writeln(Y),
    ml(Y, Z),
    writeln([ml]),
    writeln(Z),
    writeln([html]),
    html(math(Z)).

example :- example1(underbrace(n, space("instead of", x))).
example :- example1(a - b - c).
example :- example1((-1) * (-1)).
example :- example1(a * b - c).
example :- example1(a - b * c).
example :- example1(-1).
example :- example1(-a).
example :- example1(-red(a)).
example :- example1(-(a-b)).
example :- example1(a - (b - c)).
example :- example1(a - red(b - c)).
example :- example1((a - b) - c).
example :- example1(red(a - b) - c).
example :- example1(sqrt('N')).
example :- example1(dfrac(factorial(n), factorial(x)*cancel(factorial(n-x)))).
example :- example1(dfrac(factorial(n), factorial(x)*factorial(n-x))).
example :- example1(dfrac('Sum'(i, 1, 'N', (sub(x, i) - overline('X'))^2), 'N'-1)).
example :- example1(a - (b - (c - (d - (e - f))))).
example :- example1(a / (b / c)).
example :- example1(a / b * c).
example :- example1(a - (b - c)).
example :- example1(a - (b + c)).
example :- example1(a + (b + c)).
example :- example1(a + (b - c)).
example :- example1(a - b - c).
example :- example1(a * (c + d)).
example :- example1((a + b) * c).
example :- example1((a + b) * (c + d)).
example :- example1(paren(paren(1))).
example :- example1(paren(dfrac(1, paren(paren(paren(x)))))).
example :- example1(choose(n, x) * pi^x * (1-pi)^(n-x)).
example :- example1((a ; b)).
example :- example1(function(f, ((a, b) ; (c, d)))).
example :- example1(cos(x)/2).
example :- example1(sin(cos(x))).
example :- example1(sin(b)).
example :- example1(sin(b^2)).
example :- example1(sin(b)^2).
example :- example1(sin(b^2)^2).
example :- example1(sin(b / c)^2).
example :- example1(sin(b / c)).
example :- example1(sin(alpha) * cos(alpha)).
example :- example1(ubinom(k, n, pi=frac(1, 2))).
example :- example1(pbinom(k, n, pi=frac(1, 2))).
example :- example1(dbinom(k, n, pi=frac(1, 2))).
