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
:- discontiguous mml/3.
:- discontiguous example/1.
:- discontiguous paren/3.
:- discontiguous type/2.
:- discontiguous prec/4.
:- discontiguous inner/3.

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
    mathml([error-show], A, X).

mathml(Flags, A, X) :-
    denoting(Flags, A, []),
    !, mml(Flags, A, M),
    X = math(M).

mathml(Flags, A, math([mrow([M, ',']), mspace(width(thickmathspace), '') | D])) :-
    mml(Flags, A, M),
    denoting(Flags, A, D).

% non swish
html(X) :-
    html(X, Tokens, []), print_html(Tokens).

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

mml(_Flags, A, X) :-
    type(identifier, A),
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

mml(_Flags, A, X) :-
    type(punct, A),
    punct(A, M),
    !, X = mi(M).

punct(' ', \['&nbsp;']).
punct(ldots, \['&hellip;']).

% Operator precedence
prec(_Flags, A, P, Op) :-
    type(atomic, A),
    mi(A, _),
    !, P = 0,
    Op = mi.

% Depth of parentheses
paren(_Flags, A, 0) :-
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

mml(_Flags, A, X) :-
    type(operator, A),
    mo(A, M),
    !, X = mo(M).

mo(*, \['&sdot;']).
mo('.', \['&sdot;']).
mo(/, /).
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

mml(_Flags, A, X) :-
    type(operator, A),
    current_op(_, _, A),
    !, X = mo(A).

%
% Other atoms
%
type(identifier, X) :-
    atom(X),
    \+ type(punct, X),
    \+ type(operator, X).

mml(_Flags, A, X) :-
    atom(A),
    !, X = mi(A).

% Some tests
example(var) :-
    mathml(a + alpha, M),
    html(M).

%
% Symbols (non-italicized)
%
mml(_Flags, &(badbreak), mspace([width("0ex"), linebreak(badbreak)], '')) :-
    !.

mml(_Flags, &(sp), mspace([width("0.5ex"), linebreak(goodbreak)], '')) :-
    !.

mml(_Flags, &(A), mtext(&(X))) :-
    atom(A),
    !, atom_string(X, A).

mml(_Flags, A, mtext(X)) :-
    (   string(A); atom(X)
    ), !,
    atom_string(X, A).

paren(_Flags, A, 0) :-
    string(A).

%
% Parentheses
%
mml(Flags, paren(A), X) :-
    paren(Flags, A, 0),
    !, mml(Flags, parenthesis(A), X).

mml(Flags, paren(A), X) :-
    paren(Flags, A, 1),
    !, mml(Flags, bracket(A), X).

mml(Flags, paren(A), X) :-
    !, mml(Flags, curly(A), X).

% Determine level
paren(Flags, paren(A), P) :-
    !, paren(Flags, A, P0),
    P is P0 + 1.

paren(_Flags, parenthesis(_), P) :-
    !, P = 1.

paren(_Flags, bracket(_), P) :-
    !, P = 2.

paren(_Flags, curly(_), P) :-
    !, P = 3.

mml(Flags, parenthesis(A), mfenced(X)) :-
    mml(Flags, A, X).

mml(Flags, bracket(A), mfenced([open('['), close(']')], X)) :-
    mml(Flags, A, X).

mml(Flags, curly(A), mfenced([open('{'), close('}')], X)) :-
    mml(Flags, A, X).

example(par) :-
    mathml(paren(1), M),
    html(M).

%
% Lists (e.g., function arguments)
%
mml(Flags, [H | T], X) :-
    !, mml(Flags, list([H | T], ', '), X).

paren(Flags, [H | T], P) :-
    !, paren(Flags, list([H | T], sep), P).

paren(Flags, list([H | T], _Sep), P) :-
    !, maplist({Flags}/[X, Y] >> paren(Flags, X, Y), [H | T], Pi),
    sort(0, @>, Pi, [P | _]).

mml(Flags, list([H | T]), X) :-
    !, mml(Flags, list([H | T], ''), X).

mml(Flags, list([H | T], Sep), mfenced([open(''), close(''), separators(Sep)], Y)) :-
    !, maplist({Flags}/[A, X] >> mml(Flags, A, X), [H | T], Y).

example(list) :-
    mathml([1, 2, 3], M),
    html(M).

%
% Fractions
%
mml(Flags, frac(A, B), mfrac([X, Y])) :-
    !, mml(Flags, A, X),
    mml(Flags, B, Y).

mml(Flags, dfrac(A, B), mstyle(displaystyle(true), X)) :-
    !, mml(Flags, frac(A, B), X).

paren(_Flags, frac(_, _), 0) :-
    !.

paren(Flags, dfrac(A, B), P) :-
    !, paren(Flags, frac(A, B), P).

prec(_Flags, frac(_, _), P, Op) :-
    !, current_op(P, yfx, /),
    Op = (/).

prec(Flags, dfrac(A, B), P, Op) :-
    !, prec(Flags, frac(A, B), P, Op).

example(frac) :-
    mathml(frac(1.5, 2)^2, M),
    html(M).

example(frac) :-
    mathml(frac(small, small) = dfrac(large, large), M),
    html(M).

%
% Functions
%
mml(Flags, fun(Name, Args_Params), X) :-
    mml(Flags, Name apply_function paren(Args_Params), X).

mml(Flags, no_paren(Name, Arg), X) :-
    type(atomic, Arg),
    !, mml(Flags, Name apply_function Arg, X).

mml(Flags, no_paren(Name, Arg), X) :-
    type(positive, Arg), 
    !, mml(Flags, Name apply_function Arg, X).

mml(Flags, no_paren(Name, Arg), X) :-
    mml(Flags, fun(Name, Arg), X).

mml(Flags, dbinom(K, N, P), X) :-
    mml(Flags, fun('P' '_' "Bi", 'X' = K ; [N, P]), X).

mml(Flags, pbinom(K, N, P), X) :-
    mml(Flags, fun('P' '_' "Bi", 'X' =< K ; [N, P]), X).

mml(Flags, ubinom(K, N, P), X) :-
    mml(Flags, fun('P' '_' "Bi", 'X' >= K ; [N, P]), X).

mml(Flags, qbinom(Alpha, N, P), X) :-
    mml(Flags, fun('Q' '_' "Bi", Alpha ; [N, P]), X).

mml(Flags, uqbinom(Alpha, N, P), X) :-
    mml(Flags, fun('Q' '_' "Bi", 1-Alpha ; [N, P]), X).

% Bit unusual terminology
mml(Flags, bernoulli(Succ, N, Pi), X) :-
    mml(Flags, successes(Succ, Pi) * failures(N-Succ, Pi), X).

mml(Flags, successes(Succ, Pi), X) :-
    mml(Flags, Succ^Pi, X).

mml(Flags, failures(Fail, Pi), X) :-
    mml(Flags, Fail^(1-Pi), X).

mml(Flags, sqrt(A), msqrt(X)) :-
    mml(Flags, A, X).

paren(_Flags, sqrt(_), P) :-
    !, P = 0.

mml(Flags, choose(N, K), mfenced(mfrac([linethickness(0)], [X, Y]))) :-
    mml(Flags, N, X),
    mml(Flags, K, Y).

paren(_Flags, choose(_, _), P) :-
    !, P = 1.

mml(Flags, 'Sum'(I, From, To, A), mrow([munderover([\['&sum;'], XIFrom, XTo]), X])) :-
    mml(Flags, I = From, XIFrom),
    mml(Flags, To, XTo),
    mml(Flags, A, X).

paren(Flags, 'Sum'(_, _, _, A), P) :-
    !, paren(Flags, A, P).

prec(_Flags, 'Sum'(_, _, _, _), P, Op) :-
    !, current_op(P, yfx, +),
    Op = (+).

inner(Flags, 'Sum'(A, B, C, D), P) :-
    prec(Flags, 'Sum'(A, B, C, D), Prec, +),
    !, P is Prec + 1.

mml(Flags, argmin(A, _, _, B), X) :-
    mml(Flags, under("argmin", A) apply_function B, X).

mml(Flags, argmax(A, _, _, B), X) :-
    mml(Flags, fun(under("argmax", A), B), X).

mml(Flags, under(A, B), munder([X, Y])) :-
    mml(Flags, A, X),
    mml(Flags, B, Y).

% Trigonometric functions
mml(Flags, sin(A), X) :-
    mml(Flags, no_paren(sin, A), X).

mml(Flags, cos(A), X) :-
    mml(Flags, no_paren(cos, A), X).

mml(Flags, tan(A), X) :-
    mml(Flags, no_paren(tan, A), X).

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
mml(Flags, red(X), Y) :-
    !, mml(Flags, color(red, X), Y).

mml(Flags, green(X), Y) :-
    !, mml(Flags, color(green, X), Y).

mml(Flags, blue(X), Y) :-
    !, mml(Flags, color(blue, X), Y).

mml(Flags, black(X), Y) :-
    !, mml(Flags, color(black, X), Y).

mml(Flags, overline(X), mover(accent(true), [Y, mo(stretchy(true), &('OverBar'))])) :-
    !, mml(Flags, X, Y).

mml(Flags, color(Col, X), mstyle(color(Col), Y)) :-
    !, mml(Flags, X, Y).

mml(Flags, underbrace(X, Under), munder([munder([accentunder(true)],
        [Y, mo([stretchy(true)], \['&UnderBrace;'])]), New])) :-
    mml(Flags, X, Y),
    mml(Flags, Under, New).

paren(Flags, red(X), P) :-
    !, paren(Flags, X, P).

paren(Flags, green(X), P) :-
    !, paren(Flags, X, P).

paren(Flags, blue(X), P) :-
    !, paren(Flags, X, P).

paren(Flags, black(X), P) :-
    !, paren(Flags, X, P).

paren(Flags, overline(X), P) :-
    !, paren(Flags, X, P).

paren(Flags, color(_, X), P) :-
    !, paren(Flags, X, P).

paren(Flags, underbrace(X, _), P) :-
    !, paren(Flags, X, P).

prec(Flags, red(X), P, Op) :-
    !, prec(Flags, X, P, Op).

prec(Flags, green(X), P, Op) :-
    !, prec(Flags, X, P, Op).

prec(Flags, blue(X), P, Op) :-
    !, prec(Flags, X, P, Op).

prec(Flags, black(X), P, Op) :-
    !, prec(Flags, X, P, Op).

prec(Flags, overline(X), P, Op) :-
    !, prec(Flags, X, P, Op).

prec(Flags, color(_, X), P, Op) :-
    !, prec(Flags, X, P, Op).

prec(Flags, underbrace(X, _), P, Op) :-
    !, prec(Flags, X, P, Op).

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
% Mistakes
% 
mml(Flags, instead_of(A, _B), X) :-
    member(error-show, Flags),
    !, mml(Flags, red(A), X).

mml(Flags, instead_of(_A, B), X) :-
    member(error-fix, Flags),
    !, mml(Flags, green(B), X).

mml(Flags, instead_of(A, B), X) :-
    member(error-highlight, Flags),
    !, mml(Flags, underbrace(A, list(["instead of", ' ', B])), X).

paren(Flags, instead_of(A, _B), P) :-
    member(error-show, Flags),
    !, paren(Flags, A, P).

paren(Flags, instead_of(_A, B), P) :-
    member(error-fix, Flags),
    !, paren(Flags, B, P).

paren(Flags, instead_of(A, _B), P) :-
    member(error-highlight, Flags),
    !, paren(Flags, A, P).

prec(Flags, instead_of(A, _B), P, Op) :-
    member(error-show, Flags),
    !, prec(Flags, A, P, Op).

prec(Flags, instead_of(_A, B), P, Op) :-
    member(error-fix, Flags),
    !, prec(Flags, B, P, Op).

prec(Flags, instead_of(A, _B), P, Op) :-
    member(error-highlight, Flags),
    !, prec(Flags, A, P, Op).

%
% Abbreviations
%
mml(Flags, with(X, _, _), Y) :-
    mml(Flags, X, Y).

paren(Flags, with(A, _, _), P) :-
    !, paren(Flags, A, P).

prec(Flags, with(A, _, _), P, Op) :-
    !, prec(Flags, A, P, Op).

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
mml(Flags, A '_' B ^ C, msubsup([X, Y, Z])) :-
    !, current_op(Sub, xfy, '_'),
    mml_paren(Flags, Sub, A, X),
    mml(Flags, B, Y),
    mml(Flags, C, Z).

type(atomic, _^B) :-
    type(atomic, B).

% No parentheses around exponent
mml(Flags, A^B, msup([X, Y])) :-
    !, current_op(Power, xfy, ^),
    mml_paren(Flags, Power, A, X),
    mml(Flags, B, Y).

% No parentheses around subscript
mml(Flags, A '_' B, msub([X, Y])) :-
    !, current_op(Power, xfy, '_'),
    mml_paren(Flags, Power, A, X),
    mml(Flags, B, Y).

% Omit dot for multiplication
mml(Flags, A * B, X) :-
    invisible_times(A * B),
    !, mml(Flags, A invisible_times B, X).

% Postfix operators (e.g., factorial)
mml(Flags, Prod, mrow([X, Sign])) :-
    compound(Prod),
    compound_name_arguments(Prod, Op, [A]),
    current_op(Prec, Fix, Op),
    member(Fix, [xf, yf]),
    !, mml_paren(Flags, Prec, A, X),
    mml(Flags, Op, Sign).

type(atomic, N!) :-
    type(atomic, N).

% Prefix operators (e.g., factorial)
mml(Flags, Prod, mrow([Sign, X])) :-
    compound(Prod),
    compound_name_arguments(Prod, Op, [A]),
    current_op(Prec, Fix, Op),
    member(Fix, [fx, fy]),
    !, mml(Flags, Op, Sign),
    mml_paren(Flags, Prec, A, X).

% Binary operators
mml(Flags, Prod, mrow([X, Sign, Y])) :-
    compound(Prod),
    compound_name_arguments(Prod, Op, [A, B]),
    current_op(Prec, Fix, Op),
    member(Fix, [yfx, xfx, xfy]),
    !, mml_paren(Flags, Prec, A, X),
    mml(Flags, Op, Sign),
    mml_paren(Flags, Prec, B, Y).

%
% Parenthesis in (a - b) * ...
%
mml_paren(Flags, Prod, A, X) :-
    inner(Flags, A, Minus),
    Prod < Minus,
    !, mml(Flags, paren(A), X).

mml_paren(Flags, _, A, X) :-
    mml(Flags, A, X).

paren(Flags, A '_' _ ^ _, P) :-
    !, paren(Flags, A, P).

paren(Flags, A '_' _, P) :-
    !, paren(Flags, A, P).

paren(Flags, A ^ _, P) :-
    !, paren(Flags, A, P).

paren(Flags, A, P) :-
    compound(A),
    compound_name_arguments(A, Op, [Arg]),
    current_op(Prec, Fix, Op),
    member(Fix, [xf, yf, fx, fy]),
    !, paren(Flags, Arg, P0),
    (   inner(Flags, Arg, Plus), Prec < Plus -> P is P0 + 1 ; P is P0   ).

paren(Flags, A, P) :-
    compound(A),
    compound_name_arguments(A, Op, [Arg1, Arg2]),
    current_op(Prec, Fix, Op),
    member(Fix, [xfx, xfy, yfx]),
    !, paren(Flags, Arg1, P1),
    paren(Flags, Arg2, P2),
    (   inner(Flags, Arg1, Plus1), Prec < Plus1 -> P11 is P1 + 1 ; P11 is P1   ),
    (   inner(Flags, Arg1, Plus2), Prec < Plus2 -> P22 is P2 + 1 ; P22 is P2   ),
    P is max(P11, P22).

paren(Flags, X, P) :-
    compound(X),
    compound_name_arguments(X, _, Args),
    !, paren(Flags, Args, Pi), % see below "Lists"
    P is Pi + 1.

%
% Operator precedence
%
% use binary operator instead of "sign"
prec(_Flags, -_, Prec, -) :-
    !, current_op(Prec, yfx, -).

prec(_Flags, X, P, O) :-
    compound(X),
    compound_name_arity(X, Op, Arity),
    current_op(Prec, Fix, Op),
    member(Fix-Arity, [xf-1, fx-1, yf-1, fy-1, xfx-2, yfx-2, xfy-2]),
    !, P = Prec,
    O = Op.

% Parentheses in ... - (1 - 1)
inner(Flags, X, P) :-
    prec(Flags, X, Prec, Op),
    member(Fix-Op, [yfx-(-), yfx-(/), yf-(factorial)]),
    current_op(Prec, Fix, Op),
    !, P is Prec + 1.

% General case
inner(Flags, X, P) :-
    prec(Flags, X, P, _).

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
mml(_Flags, A, mn(A)) :-
    number(A),
    A >= 0.

mml(Flags, A, X) :-
    number(A),
    A < 0,
    Abs is abs(A),
    mml(Flags, -Abs, X).

paren(_Flags, A, P) :-
    type(number, A), 
    !, P = 0.

% Operator precedence
prec(_Flags, A, P, O) :-
    type(positive, A),
    !, P = 0,
    O = number.

prec(_Flags, A, P, O) :-
    type(negative, A),
    A < 0,
    !, current_op(P, yfx, -), % use binary operator
    O = (-).

example(num) :-
    mathml(1.5 + (-1.5), M),
    html(M).

% Collect abbreviations
with(Flags, with(Abbrev, Exp, Desc), X) :-
    !, with(Flags, Exp, T),
    X = [with(Abbrev, Exp, Desc) | T].

with(_Flags, X, W) :-
    atomic(X),
    !, W = [].

with(Flags, instead_of(X, Y, _Desc), W) :-
    member(error-highlight, Flags),
    !, with(Flags, X, WX),
    with(Flags, Y, WY),
    append(WX, WY, W).

with(Flags, instead_of(X, _Y), W) :-
    member(error-show, Flags),
    !, with(Flags, X, W).

with(Flags, instead_of(_X, Y), W) :-
    member(error-fix, Flags),
    !, with(Flags, Y, W).

with(Flags, X, W) :-
    compound(X),
    compound_name_arguments(X, _, Args),
    maplist({Flags}/[A, L] >> with(Flags, A, L), Args, List),
    append(List, W).

% Render abbreviations
denoting(Flags, A, []) :-
    with(Flags, A, []),
    !.

denoting(Flags, A, M) :-
    with(Flags, A, with(Abbrev, Exp, Desc)),
    !, mml(Flags, list([&(sp), "with", &(sp), Abbrev = Exp, &(sp), "denoting", &(sp), Desc, "."]), M).

denoting(Flags, A, [M | MT]) :-
    with(Flags, A, [with(Abbrev, Exp, Desc) | T]),
    mml(Flags, list(["with", &(sp), Abbrev = Exp, &(sp), "denoting", &(sp), Desc, ",", &(sp)]), M),
    denoting_and(Flags, T, MT).

denoting_and(Flags, [], [X]) :-
    mml(Flags, ".", X).

denoting_and(Flags, [with(Abbrev, Exp, Desc) | T], [M | MT]) :-
    mml(Flags, list(["and", &(sp), Abbrev = Exp, &(sp), "denoting", &(sp), Desc]), M),
    denoting_and(Flags, T, MT).

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
