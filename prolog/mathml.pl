:- module(mathml, [
    mathml/2,
    op(300, xfx, apply_function),
    op(400, yfx, invisible_times),
    op(180, xf, !),
    op(200, xfy, '_')]).

:- op(300, xfx, apply_function).
:- op(400, yfx, invisible_times).
:- op(180, xf, !).
:- op(200, xfy, '_').

:- use_module(library(http/html_write)).
:- discontiguous mml/2.
:- discontiguous example/1.
:- discontiguous paren/2.
:- discontiguous type/2.
:- discontiguous prec/3.
:- discontiguous inner/2.

%
% Interface
%
% invoke with something like
%
% mathml(dbinom(k, 'N', pi) =
%     with(choose('N', k) = dfrac('N'!, k!*('N'-k)!), "the binomial coefficient")
%     * pi^k * (1-pi)^('N' - k), M), html(M).
%
mathml(A, X) :-
    denoting(A, []),
    !, mml(A, M),
    X = math(M).

mathml(A, math([mrow([M, ',']), mspace(width(thickmathspace), '') | D])) :-
    mml(A, M),
    denoting(A, D).

%
% Check types
%
% Accepts compounds like red(2) as numbers
type(number, X) :-
    number(X).

type(positive, X) :-
    number(X), 
    X >= 0.

type(negative, X) :-
    number(X), 
    X < 0.

type(atomic, X) :-
    type(positive, X).

type(atomic, X) :-
    type(identifier, X).

type(atomic, X) :-
    type(punct, X).

type(atomic, X) :-
    type(operator, X).

%
% Variables ('identifiers')
%
type(identifier, X) :-
    atom(X),
    mi(X, _).

mml(A, X) :-
    atom(A),
    mi(A, M),
    !, X = mi(M).

mi(alpha, \['&alpha;']).
mi(mu, \['&mu;']).
mi(pi, \['&pi;']).
mi(sigma, \['&sigma;']).

%
% Punctuation
%
type(punct, X) :-
    atom(X),
    punct(X, _).

mml(A, X) :-
    atom(A),
    punct(A, M),
    !, X = mi(M).

punct(ldots, \['&hellip;']).

% Operator precedence
prec(A, P, Op) :-
    type(atomic, A),
    mi(A, _),
    !, P = 0,
    Op = mi.

% Depth of parentheses
paren(A, 0) :-
    type(atomic, A).

%
% Operators
%
type(operator, X) :-
    atom(X),
    mo(X, _).

type(operator, X) :-
    atom(X),
    current_op(_, _, X).

mml(A, X) :-
    atom(A),
    mo(A, M),
    !, X = mo(M).

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
mo(!, !).
mo(cdots, \['&ctdot;']).
mo(',', [separator(true), \[',']]).
mo(';', \['&#59;']).
mo('|', \['|']).
mo(invisible_times, \['&#x2062']).
mo(apply_function, \['&ApplyFunction;']).

mml(A, X) :-
    atom(A),
    current_op(_, _, A),
    !, X = mo(A).

%
% Other atoms
%
type(identifier, X) :-
    atom(X),
    \+ type(punct, X),
    \+ type(operator, X).

mml(A, X) :-
    atom(A),
    !, X = mi(A).

% Some tests
example(var) :-
    mathml(a + alpha, M),
    html(M).

%
% Symbols (non-italicized)
%
mml(&(badbreak), mspace([width("0ex"), linebreak(badbreak)], '')) :-
    !.

mml(&(sp), mspace([width("0.5ex"), linebreak(goodbreak)], '')) :-
    !.

mml(&(A), mtext(&(X))) :-
    atom(A),
    !, atom_string(X, A).

mml(A, mtext(X)) :-
    (   string(A); atom(X)
    ), !,
    atom_string(X, A).

paren(A, 0) :-
    string(A).

%
% Parentheses
%
mml(paren(A), X) :-
    paren(A, 0),
    !, mml(parenthesis(A), X).

mml(paren(A), X) :-
    paren(A, 1),
    !, mml(bracket(A), X).

mml(paren(A), X) :-
    !, mml(curly(A), X).

% Determine level
paren(paren(A), P) :-
    !, paren(A, P0),
    P is P0 + 1.

paren(parenthesis(_), P) :-
    !, P = 1.

paren(bracket(_), P) :-
    !, P = 2.

paren(curly(_), P) :-
    !, P = 3.

mml(parenthesis(A), mfenced(X)) :-
    mml(A, X).

mml(bracket(A), mfenced([open('['), close(']')], X)) :-
    mml(A, X).

mml(curly(A), mfenced([open('{'), close('}')], X)) :-
    mml(A, X).

example(par) :-
    mathml(paren(1), M),
    html(M).

%
% Lists (e.g., function arguments)
%
mml([H | T], X) :-
    !, mml(list([H | T], ', '), X).

paren([H | T], P) :-
    !, paren(list([H | T], sep), P).

paren(list([H | T], _Sep), P) :-
    !, maplist(paren, [H | T], Pi),
    sort(0, @>, Pi, [P | _]).

mml(list([H | T]), X) :-
    !, mml(list([H | T], ''), X).

mml(list([H | T], Sep), mfenced([open(''), close(''), separators(Sep)], X)) :-
    !, maplist(mml, [H | T], X).

example(list) :-
    mathml([1, 2, 3], M),
    html(M).

%
% Fractions
%
mml(frac(A, B), mfrac([X, Y])) :-
    !, mml(A, X),
    mml(B, Y).

mml(dfrac(A, B), mstyle(displaystyle(true), X)) :-
    !, mml(frac(A, B), X).

paren(frac(_, _), 0) :-
    !.

paren(dfrac(A, B), P) :-
    !, paren(frac(A, B), P).

prec(frac(_, _), P, Op) :-
    !, current_op(P, yfx, /),
    Op = (/).

prec(dfrac(A, B), P, Op) :-
    !, prec(frac(A, B), P, Op).

example(frac) :-
    mathml(frac(1.5, 2)^2, M),
    html(M).

example(frac) :-
    mathml(frac(small, small) = dfrac(large, large), M),
    html(M).

%
% Functions
%
mml(fun(Name, Args_Params), X) :-
    mml(Name apply_function paren(Args_Params), X).

mml(no_paren(Name, Arg), X) :-
    type(atomic, Arg),
    !, mml(Name apply_function Arg, X).

mml(no_paren(Name, Arg), X) :-
    type(positive, Arg), 
    !, mml(Name apply_function Arg, X).

mml(no_paren(Name, Arg), X) :-
    mml(fun(Name, Arg), X).

mml(dbinom(K, N, P), X) :-
    mml(fun('P' '_' "Bi", 'X' = K ; [N, P]), X).

mml(pbinom(K, N, P), X) :-
    mml(fun('P' '_' "Bi", 'X' =< K ; [N, P]), X).

mml(ubinom(K, N, P), X) :-
    mml(fun('P' '_' "Bi", 'X' >= K ; [N, P]), X).

mml(qbinom(Alpha, N, P), X) :-
    mml(fun('Q' '_' "Bi", Alpha ; [N, P]), X).

mml(uqbinom(Alpha, N, P), X) :-
    mml(fun('Q' '_' "Bi", 1-Alpha ; [N, P]), X).

% Bit unusual terminology
mml(bernoulli(Succ, N, Pi), X) :-
    mml(successes(Succ, Pi) * failures(N-Succ, Pi), X).

mml(successes(Succ, Pi), X) :-
    mml(Succ^Pi, X).

mml(failures(Fail, Pi), X) :-
    mml(Fail^(1-Pi), X).

mml(sqrt(A), msqrt(X)) :-
    mml(A, X).

paren(sqrt(_), 0) :-
    !.

mml(choose(N, K), mfenced(mfrac([linethickness(0)], [X, Y]))) :-
    mml(N, X),
    mml(K, Y).

paren(choose(_, _), 1) :-
    !.

mml('Sum'(I, From, To, A), mrow([munderover([\['&sum;'], XIFrom, XTo]), X])) :-
    mml(I = From, XIFrom),
    mml(To, XTo),
    mml(A, X).

paren('Sum'(_, _, _, A), P) :-
    !, paren(A, P).

prec('Sum'(_, _, _, _), P, Op) :-
    !, current_op(P, yfx, +),
    Op = (+).

inner('Sum'(A, B, C, D), P) :-
    prec('Sum'(A, B, C, D), Prec, +),
    !, P is Prec + 1.

mml(argmin(A, _, _, B), X) :-
    mml(under("argmin", A) apply_function B, X).

mml(argmax(A, _, _, B), X) :-
    mml(fun(under("argmax", A), B), X).

mml(under(A, B), munder([X, Y])) :-
    mml(A, X),
    mml(B, Y).

% Trigonometric functions
mml(sin(A), X) :-
    mml(no_paren(sin, A), X).

mml(cos(A), X) :-
    mml(no_paren(cos, A), X).

mml(tan(A), X) :-
    mml(no_paren(tan, A), X).

example(fun) :-
    mathml(sin(alpha), M),
    html(M).

example(fun) :-
    mathml(sin(30), M),
    html(M).

example(fun) :-
    mathml(qbinom(alpha, 'N', pi) =
        argmax(k, 1, 10, pbinom(k, 'N', pi) =< alpha), M),
    html(M).

example(fun) :-
    mathml(choose('N', k) = frac('N'!, k! * ('N' - k)!), M),
    html(M).

example(fun) :-
    mathml(choose('N', k) = frac('N'!, k! * ('N' - k)!), M),
    html(M).

example(fun) :-
    mathml(dbinom(k, 'N', 'pi'), M),
    html(M).

example(fun) :-
    mathml(pbinom(k, 'N', 'pi'), M),
    html(M).

example(fun) :-
    mathml(ubinom(k, 'N', 'pi'), M),
    html(M).

example(fun) :-
    mathml(qbinom(alpha, 'N', 'pi'), M),
    html(M).

example(fun) :-
    mathml(uqbinom(alpha, 'N', 'pi'), M),
    html(M).

example(fun) :-
    mathml(bernoulli(k, 'N', 'pi'), M),
    html(M).

example(fun) :-
    mathml(a * (1 + sqrt(paren(paren(k)))), M),
    html(M).

example(fun) :-
    mathml(pbinom(k, 'N', pi) = 'Sum'(i, k, 'N', choose('N', k) *
        fun('P' '_' "Bi", 'X' = k; ['N', pi])), M),
    html(M).

example(fun) :-
    mathml('Sum'(i, k, 'N', i) + 'Sum'(i, k, 'N', i), M),
    html(M).

%
% Decorations
%
mml(red(X), Y) :-
    !, mml(color(red, X), Y).

mml(green(X), Y) :-
    !, mml(color(green, X), Y).

mml(blue(X), Y) :-
    !, mml(color(blue, X), Y).

mml(black(X), Y) :-
    !, mml(color(black, X), Y).

mml(color(Col, X), mstyle(color(Col), Y)) :-
    !, mml(X, Y).

paren(red(X), P) :-
    !, paren(X, P).

paren(green(X), P) :-
    !, paren(X, P).

paren(blue(X), P) :-
    !, paren(X, P).

paren(black(X), P) :-
    !, paren(X, P).

paren(color(_, X), P) :-
    !, paren(X, P).

prec(red(X), P, Op) :-
    !, prec(X, P, Op).

prec(green(X), P, Op) :-
    !, prec(X, P, Op).

prec(blue(X), P, Op) :-
    !, prec(X, P, Op).

prec(black(X), P, Op) :-
    !, prec(X, P, Op).

prec(color(_, X), P, Op) :-
    !, prec(X, P, Op).

type(T, red(X)) :-
    type(T, X).

type(T, green(X)) :-
    type(T, X).

type(T, blue(X)) :-
    type(T, X).

type(T, black(X)) :-
    type(T, X).

type(T, color(_, X)) :-
    type(T, X).

example(col) :-
    mathml(a * red(b + c), M),
    html(M).

%
% Operators
%
% check if multiplication sign can be omitted
%
% Todo: clean up and include compounds such as A^B, red(A), with(A, _, _)
invisible_times(A) :-
    type(identifier, A).

invisible_times(A) :-
    \+ type(number, A),
    \+ type(punct, A),
    type(atomic, A).

invisible_times(A*B) :-
    invisible_times_left(A),
    invisible_times(B).

invisible_times_left(A) :-
    type(number, A).

invisible_times_left(A) :-
    invisible_times(A).

% No parentheses around base in subscript and powers
mml(A '_' B ^ C, msubsup([X, Y, Z])) :-
    !, current_op(Sub, xfy, '_'),
    mml_paren(Sub, A, X),
    mml(B, Y),
    mml(C, Z).

type(atomic, _^B) :-
    type(atomic, B).

% No parentheses around exponent
mml(A^B, msup([X, Y])) :-
    !, current_op(Power, xfy, ^),
    mml_paren(Power, A, X),
    mml(B, Y).

% No parentheses around subscript
mml(A '_' B, msub([X, Y])) :-
    !, current_op(Power, xfy, '_'),
    mml_paren(Power, A, X),
    mml(B, Y).

% Omit dot for multiplication
mml(A * B, X) :-
    invisible_times(A * B),
    !, mml(A invisible_times B, X).

% Postfix operators (e.g., factorial)
mml(Prod, mrow([X, Sign])) :-
    compound(Prod),
    compound_name_arguments(Prod, Op, [A]),
    current_op(Prec, Fix, Op),
    member(Fix, [xf, yf]),
    !, mml_paren(Prec, A, X),
    mml(Op, Sign).

type(atomic, N!) :-
    type(atomic, N).

% Prefix operators (e.g., factorial)
mml(Prod, mrow([Sign, X])) :-
    compound(Prod),
    compound_name_arguments(Prod, Op, [A]),
    current_op(Prec, Fix, Op),
    member(Fix, [fx, fy]),
    !, mml(Op, Sign),
    mml_paren(Prec, A, X).

% Binary operators
mml(Prod, mrow([X, Sign, Y])) :-
    compound(Prod),
    compound_name_arguments(Prod, Op, [A, B]),
    current_op(Prec, Fix, Op),
    member(Fix, [yfx, xfx, xfy]),
    !, mml_paren(Prec, A, X),
    mml(Op, Sign),
    mml_paren(Prec, B, Y).

%
% Parenthesis in (a - b) * ...
%
mml_paren(Prod, A, X) :-
    inner(A, Minus),
    Prod < Minus,
    !, mml(paren(A), X).

mml_paren(_, A, X) :-
    mml(A, X).

paren(A '_' _ ^ _, P) :-
    !, paren(A, P).

paren(A '_' _, P) :-
    !, paren(A, P).

paren(A ^ _, P) :-
    !, paren(A, P).

paren(A, P) :-
    compound(A),
    compound_name_arguments(A, Op, [Arg]),
    current_op(Prec, Fix, Op),
    member(Fix, [xf, yf, fx, fy]),
    !, paren(Arg, P0),
    (   inner(Arg, Plus), Prec < Plus -> P is P0 + 1 ; P is P0   ).

paren(A, P) :-
    compound(A),
    compound_name_arguments(A, Op, [Arg1, Arg2]),
    current_op(Prec, Fix, Op),
    member(Fix, [xfx, xfy, yfx]),
    !, paren(Arg1, P1),
    paren(Arg2, P2),
    (   inner(Arg1, Plus1), Prec < Plus1 -> P11 is P1 + 1 ; P11 is P1   ),
    (   inner(Arg1, Plus2), Prec < Plus2 -> P22 is P2 + 1 ; P22 is P2   ),
    P is max(P11, P22).

paren(X, P) :-
    compound(X),
    compound_name_arguments(X, _, Args),
    !, paren(Args, Pi), % see below "Lists"
    P is Pi + 1.

%
% Operator precedence
%
% use binary operator instead of "sign"
prec(-_, Prec, -) :-
    !, current_op(Prec, yfx, -).

prec(X, P, O) :-
    compound(X),
    compound_name_arity(X, Op, Arity),
    current_op(Prec, Fix, Op),
    member(Fix-Arity, [xf-1, fx-1, yf-1, fy-1, xfx-2, yfx-2, xfy-2]),
    !, P = Prec,
    O = Op.

% Parentheses in ... - (1 - 1)
inner(X, P) :-
    prec(X, Prec, Op),
    member(Fix-Op, [yfx-(-), yfx-(/), yf-(factorial)]),
    current_op(Prec, Fix, Op),
    !, P is Prec + 1.

% General case
inner(X, P) :-
    prec(X, P, _).

example(op) :-
    mathml(1 + 1, M),
    html(M).

example(op) :-
    mathml(-1 + -1, M),
    html(M).

example(op) :-
    mathml(a - b - (c - d), M),
    html(M).

example(op) :-
    mathml((a - b)^(c - d), M),
    html(M).

example(op) :-
    mathml(a^b^c, M),
    html(M).

example(op) :-
    mathml((a+b) '_' (c+d), M),
    html(M).

example(op) :-
    mathml((a+b) '_' c ^ d, M),
    html(M).

example(op) :-
    mathml(paren(2*a*b*(a+b)), M),
    html(M).

%
% Numbers
%
mml(A, mn(A)) :-
    number(A),
    A >= 0.

mml(A, X) :-
    number(A),
    A < 0,
    Abs is abs(A),
    mml(-Abs, X).

paren(A, 0) :-
    type(number, A), !.

% Operator precedence
prec(A, P, O) :-
    type(positive, A),
    !, P=0,
    O = number.

prec(A, P, O) :-
    type(negative, A),
    A < 0,
    !, current_op(P, yfx, -), % use binary operator
    O = (-).

example(num) :-
    mathml(1.5 + (-1.5), M),
    html(M).

%
% Abbreviations
%
mml(with(X, _, _), Y) :-
    mml(X, Y).

paren(with(A, _, _), P) :-
    !, paren(A, P).

prec(with(A, _, _), P, Op) :-
    !, prec(A, P, Op).

% Collect abbreviations
with(with(Abbrev, Exp, Desc), X) :-
    !, with(Exp, T),
    X = [with(Abbrev, Exp, Desc) | T].

with(X, W) :-
    atomic(X),
    !, W = [].

with(X, W) :-
    compound(X),
    compound_name_arguments(X, _, Args),
    maplist(with, Args, List),
    append(List, W).

% Render abbreviations
denoting(A, []) :-
    with(A, []),
    !.

denoting(A, M) :-
    with(A, with(Abbrev, Exp, Desc)),
    !, mml(list([&(sp), "with", &(sp), Abbrev = Exp, &(sp), "denoting", &(sp), Desc, "."]), M).

denoting(A, [M | MT]) :-
    with(A, [with(Abbrev, Exp, Desc) | T]),
    mml(list(["with", &(sp), Abbrev = Exp, &(sp), "denoting", &(sp), Desc, ",", &(sp)]), M),
    denoting_and(T, MT).

denoting_and([], [X]) :-
    mml(".", X).

denoting_and([with(Abbrev, Exp, Desc) | T], [M | MT]) :-
    mml(list(["and", &(sp), Abbrev = Exp, &(sp), "denoting", &(sp), Desc]), M),
    denoting_and(T, MT).

example(ml) :-
    mathml(dbinom(k, 'N', pi) =
        with(choose('N', k),
            dfrac(
                with('N'!, 1*2*3*ldots*'N', "the factorial"),
                k!*('N'-k)!),
            "the binomial coefficient")
        * pi^k * (1-pi)^('N' - k), M),
    html(M).

example :-
    findall(true, example(_), _).
