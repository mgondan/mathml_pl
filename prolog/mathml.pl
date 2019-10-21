:- module(mathml, [
    mathml/2,
    op(300, xfx, apply_function),
    op(180, xf, !)]).

:- use_module(library(http/html_write)).

:- discontiguous mathml/2.
:- discontiguous example/0, example/1.
:- discontiguous paren/2.
:- discontiguous prec/3.
:- discontiguous inner/2.

:- current_op(Prec, yfx, *), op(Prec, yfx, invisible_times).
:- current_op(Prec, xfy, ^), op(Prec, xfy, '_').

%
% Variables ('identifiers')
%
mathml(A, X) :-
    atom(A),
    mi(A, M),
    !, X = mi(M).

mi(alpha, \['&alpha;']).
mi(mu, \['&mu;']).
mi(pi, \['&pi;']).
mi(sigma, \['&sigma;']).

% Operator precedence
prec(A, P, Op) :-
    atom(A),
    mi(A, _),
    !, P = 0,
    Op = mi.

% Depth of parentheses
paren(A, 0) :-
    atom(A).

%
% Operators
%
mathml(A, X) :-
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
mo(',', \[',']).
mo(';', \['&#59;']).
mo('|', \['|']).
mo(invisible_times, \['&#x2062']).
mo(apply_function, \['&ApplyFunction;']).

mathml(A, X) :-
    atom(A),
    current_op(_, _, A),
    !, X = mo(A).

%
% Other atoms
%
mathml(A, X) :-
    atom(A),
    !, X = mi(A).

prec(A, P, Op) :-
    atom(A),
    mi(A, _),
    !, P = 0,
    Op = mi.

% Some tests
example(var) :-
    mathml(a + alpha, M, []),
    print_html(M).

%
% Symbols (non-italicized)
%
mathml(&(A), mtext(&(X))) :-
    atom(A),
    !, atom_string(X, A).

mathml(A, mtext(X)) :-
    (   string(A); atom(X)
    ), !,
    atom_string(X, A).

paren(A, 0) :-
    string(A).

example(sym) :-
    mathml("a" + alpha, M, []),
    print_html(M).

%
% Parentheses
%
mathml(paren(A), X) :-
    paren(A, 0),
    !, mathml(parenthesis(A), X).

mathml(paren(A), X) :-
    paren(A, 1),
    !, mathml(bracket(A), X).

mathml(paren(A), X) :-
    !, mathml(curly(A), X).

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

mathml(parenthesis(A), mfenced(X)) :-
    mathml(A, X).

mathml(bracket(A), mfenced([open('['), close(']')], X)) :-
    mathml(A, X).

mathml(curly(A), mfenced([open('{'), close('}')], X)) :-
    mathml(A, X).

example(par) :-
    mathml(paren(1), M, []),
    print_html(M).

%
% Lists (e.g., function arguments)
%
mathml([H | T], X) :-
    !, mathml(list([H | T], ', '), X).

paren([H | T], P) :-
    !, paren(list([H | T], sep), P).

paren(list([H | T], _Sep), P) :-
    !, maplist(paren, [H | T], Pi),
    sort(0, @>, Pi, [P | _]).

mathml(list([H | T]), X) :-
    !, mathml(list([H | T], ''), X).

mathml(list([H | T], Sep), mfenced([open(''), close(''), separators(Sep)], X)) :-
    !, maplist(mathml, [H | T], X).

example(list) :-
    mathml([1, 2, 3], M, []),
    print_html(M).

%
% Fractions
%
mathml(frac(A, B), mfrac([X, Y])) :-
    !, mathml(A, X),
    mathml(B, Y).

mathml(dfrac(A, B), mstyle(displaystyle(true), X)) :-
    !, mathml(frac(A, B), X).

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
    mathml(frac(1.5, 2)^2, M, []),
    print_html(M).

example(frac) :-
    mathml(frac(small, small) = dfrac(large, large), M, []),
    print_html(M).

%
% Functions
%
mathml(fun(Name, Args_Params), X) :-
    mathml(Name apply_function paren(Args_Params), X).

mathml(no_paren(Name, Arg), X) :-
    atom(Arg),
    !, mathml(Name apply_function Arg, X).

mathml(no_paren(Name, Arg), X) :-
    number(Arg), Arg >= 0,
    !, mathml(Name apply_function Arg, X).

ml(no_paren(Name, Arg), X) :-
    ml(fun(Name, Arg), X).

ml(dbinom(K, N, P), X) :-
    ml(fun('P' '_' "Bi", 'X' = K ; [N, P]), X).

ml(pbinom(K, N, P), X) :-
    ml(fun('P' '_' "Bi", 'X' =< K ; [N, P]), X).

ml(ubinom(K, N, P), X) :-
    ml(fun('P' '_' "Bi", 'X' >= K ; [N, P]), X).

ml(qbinom(Alpha, N, P), X) :-
    ml(fun('Q' '_' "Bi", Alpha ; [N, P]), X).

ml(uqbinom(Alpha, N, P), X) :-
    ml(fun('Q' '_' "Bi", 1-Alpha ; [N, P]), X).

% Bit unusual terminology
mathml(bernoulli(Succ, N, Pi), X) :-
    mathml(successes(Succ, Pi) * failures(N-Succ, Pi), X).

mathml(successes(Succ, Pi), X) :-
    mathml(Succ^Pi, X).

mathml(failures(Fail, Pi), X) :-
    mathml(Fail^(1-Pi), X).

mathml(sqrt(A), msqrt(X)) :-
    mathml(A, X).

paren(sqrt(_), 0) :-
    !.

mathml(choose(N, K), mfenced(mfrac([linethickness(0)], [X, Y]))) :-
    mathml(N, X),
    mathml(K, Y).

paren(choose(_, _), 1) :-
    !.

mathml('Sum'(I, From, To, A), mrow([munderover([\['&sum;'], XIFrom, XTo]), X])) :-
    mathml(I = From, XIFrom),
    mathml(To, XTo),
    mathml(A, X).

paren('Sum'(_, _, _, A), P) :-
    !, paren(A, P).

prec('Sum'(_, _, _, _), P, Op) :-
    !, current_op(P, yfx, +),
    Op = (+).

inner('Sum'(A, B, C, D), P) :-
    prec('Sum'(A, B, C, D), Prec, +),
    !, P is Prec + 1.

mathml(argmin(A, _, _, B), X) :-
    mathml(under("argmin", A) apply_function B, X).

mathml(argmax(A, _, _, B), X) :-
    mathml(fun(under("argmax", A), B), X).

mathml(under(A, B), munder([X, Y])) :-
    mathml(A, X),
    mathml(B, Y).

% Trigonometric functions
mathml(sin(A), X) :-
    mathml(no_paren(sin, A), X).

mathml(cos(A), X) :-
    mathml(no_paren(cos, A), X).

mathml(tan(A), X) :-
    mathml(no_paren(tan, A), X).

example(fun) :-
    mathml(sin(alpha), M, []),
    print_html(M).

example(fun) :-
    mathml(qbinom(alpha, 'N', pi) =
        argmax(k, 1, 10, pbinom(k, 'N', pi) =< alpha), M, []),
    print_html(M).

example(fun) :-
    mathml(choose('N', k) = frac('N'!, k! * ('N' - k)!), M, []),
    print_html(M).

example(fun) :-
    mathml(choose('N', k) = frac('N'!, k! * ('N' - k)!), M, []),
    print_html(M).

example(fun) :-
    mathml(dbinom(k, 'N', 'pi'), M, []),
    print_html(M).

example(fun) :-
    mathml(pbinom(k, 'N', 'pi'), M, []),
    print_html(M).

example(fun) :-
    mathml(ubinom(k, 'N', 'pi'), M, []),
    print_html(M).

example(fun) :-
    mathml(qbinom(alpha, 'N', 'pi'), M, []),
    print_html(M).

example(fun) :-
    mathml(uqbinom(alpha, 'N', 'pi'), M, []),
    print_html(M).

example(fun) :-
    mathml(bernoulli(k, 'N', 'pi'), M, []),
    print_html(M).

example(fun) :-
    mathml(a * (1 + sqrt(paren(paren(k)))), M, []),
    print_html(M).

example(fun) :-
    mathml(pbinom(k, 'N', pi) = 'Sum'(i, k, 'N', choose('N', k) *
        fun('P' '_' "Bi", 'X' = k; ['N', pi])), M, []),
    print_html(M).

example(fun) :-
    mathml('Sum'(i, k, 'N', i) + 'Sum'(i, k, 'N', i), M, []),
    print_html(M).

%
% Decorations
%
mathml(red(X), Y) :-
    !, mathml(color(red, X), Y).

mathml(green(X), Y) :-
    !, mathml(color(green, X), Y).

mathml(blue(X), Y) :-
    !, mathml(color(blue, X), Y).

mathml(black(X), Y) :-
    !, mathml(color(black, X), Y).

mathml(color(Col, X), mstyle(color(Col), Y)) :-
    !, mathml(X, Y).

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

example(col) :-
    mathml(a * red(b + c), M, []),
    print_html(M).

%
% Operators
%
% check if multiplication sign can be omitted
invisible_times(A) :-
    atom(A).

invisible_times(A) :-
    number(A),
    A > 0.

invisible_times(A*B) :-
    invisible_times(A),
    invisible_times(B).

% No parentheses around base in subscript and powers
mathml(A '_' B ^ C, msubsup([X, Y, Z])) :-
    !, current_op(Sub, xfy, '_'),
    mathml_paren(Sub, A, X),
    mathml(B, Y),
    mathml(C, Z).

% No parentheses around exponent
mathml(A^B, msup([X, Y])) :-
    !, current_op(Power, xfy, ^),
    mathml_paren(Power, A, X),
    mathml(B, Y).

% No parentheses around subscript
mathml(A '_' B, msub([X, Y])) :-
    !, current_op(Power, xfy, '_'),
    mathml_paren(Power, A, X),
    mathml(B, Y).

% Omit dot for multiplication
mathml(A * B, X) :-
    invisible_times(A * B),
    !, mathml(A invisible_times B, X).

% Postfix operators (e.g., factorial)
mathml(Prod, mrow([X, Sign])) :-
    compound(Prod),
    compound_name_arguments(Prod, Op, [A]),
    current_op(Prec, Fix, Op),
    member(Fix, [xf, yf]),
    !, mathml_paren(Prec, A, X),
    mathml(Op, Sign).

% Prefix operators (e.g., factorial)
mathml(Prod, mrow([Sign, X])) :-
    compound(Prod),
    compound_name_arguments(Prod, Op, [A]),
    current_op(Prec, Fix, Op),
    member(Fix, [fx, fy]),
    !, mathml(Op, Sign),
    mathml_paren(Prec, A, X).

% Binary operators
mathml(Prod, mrow([X, Sign, Y])) :-
    compound(Prod),
    compound_name_arguments(Prod, Op, [A, B]),
    current_op(Prec, Fix, Op),
    member(Fix, [yfx, xfx, xfy]),
    !, mathml_paren(Prec, A, X),
    mathml(Op, Sign),
    mathml_paren(Prec, B, Y).

% Parenthesis in (a - b) * ...
mathml_paren(Prod, A, X) :-
    inner(A, Minus),
    Prod < Minus,
    !, mathml(paren(A), X).

mathml_paren(_, A, X) :-
    mathml(A, X).

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
    mathml(1 + 1, M, []),
    print_html(M).

example(op) :-
    mathml(-1 + -1, M, []),
    print_html(M).

example(op) :-
    mathml(a - b - (c - d), M, []),
    print_html(M).

example(op) :-
    mathml((a - b)^(c - d), M, []),
    print_html(M).

example(op) :-
    mathml(a^b^c, M, []),
    print_html(M).

example(op) :-
    mathml((a+b) '_' (c+d), M, []),
    print_html(M).

example(op) :-
    mathml((a+b) '_' c ^ d, M, []),
    print_html(M).

example(op) :-
    mathml(paren(2*a*b*(a+b)), M, []),
    print_html(M).

%
% Numbers
%
mathml(A, mn(A)) :-
    number(A),
    A >= 0.

mathml(A, X) :-
    number(A),
    A < 0,
    Abs is abs(A),
    mathml(-Abs, X).

paren(A, 0) :-
    number(A), !.

% Operator precedence
prec(A, P, number) :-
    number(A),
    A >= 0,
    !, P=0.

prec(A, P, O) :-
    number(A),
    A < 0,
    !, current_op(P, yfx, -), % use binary operator
    O = (-).

example(num) :-
    mathml(1.5 + (-1.5), M, []),
    print_html(M).

%
% Abbreviations
%
mathml(with(X, _Term, _Desc), Y) :-
    mathml(X, Y).

paren(with(A, _, _), P) :-
    !, paren(A, P).

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

% Render in HTML
denoting([]) -->
    html("").

denoting([with(ExpTerm, Desc) | T]) -->
    {mathml(list([", with", &(nbsp), ExpTerm, &(nbsp), "denoting", &(nbsp), Desc]), M)},
    html(math(M)),
    denoting_and(T).

denoting_and([]) -->
    html(".").

denoting_and([with(ExpTerm, Desc) | T]) -->
    {mathml(list([', and&(nbsp)', ExpTerm, '&(nbsp)denoting&(nbsp)', Desc], ''), M)},
    html(math(M)),
    denoting_and(T).

%
% Convert the whole thing
%
mathml(A) -->
    {
       term_string(A, S),
       mathml(A, M),
       with(A, W)
    },
    html(S),
    html(p('')),
    html(math(M)),
    denoting(W),
    html(p('')).

example(ml) :-
    mathml(((a + b)^2 = a^2 + 2*a*b + b^2), M, []),
    print_html(M).

example(ml) :-
    mathml(dbinom(k, 'N', pi) =
        with(choose('N', k), dfrac('N'!, k!*('N'-k)!), "the binomial coefficient")
           * pi^k * (1-pi)^('N' - k), M, []),
    print_html(M).

example :-
    writeln('<HTML>'),
    findall(true, example(_), _),
    writeln('</HTML>').

