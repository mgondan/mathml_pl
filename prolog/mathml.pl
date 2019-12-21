:- module(mathml, [
    mathml/2,
    mathml/3,
    mml//1,
    mml//2,
    op(400, yfx, invisible_times),
    op(180, xf, !),
    op(170, xf, '%'),
    op(200, xfy, '_'),
    op(1050, xfy, '~>')]).

:- op(180, xf, !).
:- op(170, xf, '%').
:- op(200, xfy, '_').
:- op(400, yfx, invisible_times).
:- op(1050, xfy, '~>').

:- discontiguous ml/3.
:- discontiguous paren/3.
:- discontiguous prec/3.
:- discontiguous math/3.
:- discontiguous example/0.

:- use_module(library(quantity)).
:- use_module(library(http/html_write)).

%
% Hook for custom atoms
%
% e.g., to replace s_D by sub(s, 'D')
%
% atom_hook(Flags, s_D, X) :- mathml(Flags, sub(s, 'D'), X).
%
:- multifile atom_hook/3.

%
% Interface
%
mml(A) -->
    mml([error(highlight)], A).

mml(Flags, A) -->
    { mathml(Flags, A, M) },
    html(math(M)).

mathml(A, M) :-
    mathml([error(highlight)], A, M).

mathml(Flags, A, math(X)) :-
    denoting(Flags, A, []),
    ml(Flags, A, X).

mathml(Flags, A, math([M, H | T])) :-
    denoting(Flags, A, [H | T]),
    ml(Flags, [A, ',', ' '], M).

%
% Show example
%
example(A) :-
    example([], A).

example(Flags, A) :-
    mathml(Flags, A, M)
    -> writeln(math:A = ml:M)
    ; writeln(math:A = ml:failed).

%
% For SWISH
%
% example(Flags, A) :-
%     ml(Flags, A, M) -> html(html(math(M))) ; writeln(failed).

%
% Macros (e.g., red(X) for color(red, X)
%
ml(Flags, A, M) :-
    math(Flags, A, X),
    !, ml(Flags, X, M).

paren(Flags, A, P) :-
    math(Flags, A, X),
    !, paren(Flags, X, P).

prec(Flags, A, P) :-
    math(Flags, A, X),
    !, prec(Flags, X, P).

%
% Punctuation
%
is_punctuation(Flags, A) :-
    punctuation(Flags, A, _).

punctuation(_, '_', &(nbsp)).
punctuation(_, ' ', mspace(width(thickmathspace), [])).
punctuation(_, ldots, mi(&(hellip))).
punctuation(_, cdots, mi(&(ctdot))).

ml(Flags, A, M) :-
    is_punctuation(Flags, A),
    punctuation(Flags, A, M).

paren(Flags, A, 0) :-
    is_punctuation(Flags, A).

prec(Flags, A, 0) :-
    is_punctuation(Flags, A).

example :- example(' ').
example :- example('_').

%
% Operators
%
is_operator(Flags, A) :-
    operator(Flags, A, _).

operator(_, A, mo(M)) :-
    op(A, M).

op(+, +).
op(-, -).
op(*, &(sdot)).
op(/, /).
op(=\=, &('#8203')).
op(=, =).
op(<, &(lt)).
op(>, &(gt)).
op(=<, &(le)).
op(>=, &(ge)).
op(\=, &(ne)).
op(!, !).
op('%', '%').
op(',', &(comma)).
op(';', &('#59')).
op('|', '|').
op(invisible_times, &('#x2062')).
op(->, &(rArr)).
op(~>, &(zigrarr)).

ml(Flags, A, M) :-
    is_operator(Flags, A),
    operator(Flags, A, M).

paren(Flags, A, 0) :-
    is_operator(Flags, A).

prec(Flags, A, 0) :-
    is_operator(Flags, A).

example :- example(/).

%
% Identifiers
%
is_identifier(Flags, A) :-
    identifier(Flags, A, _).

identifier(_, A, mi(M)) :-
    id(A, M).

id(alpha, &(alpha)).
id(mu, &(mu)).
id(pi, &(pi)).
id(sigma, &(sigma)).

ml(Flags, A, M) :-
    is_identifier(Flags, A),
    identifier(Flags, A, M).

paren(Flags, A, 0) :-
    is_identifier(Flags, A).

prec(Flags, A, 0) :-
    is_identifier(Flags, A).

example :- example(alpha).

%
% General atoms
%
is_atom(Flags, A) :-
    atom(Flags, A, _),
    \+ is_identifier(Flags, A),
    \+ is_punctuation(Flags, A),
    \+ is_operator(Flags, A).

atom(Flags, A, X) :-
    atom_hook(Flags, A, M),
    !, X = M.

atom(_, A, mi(A)) :-
    atom(A).

ml(Flags, A, M) :-
    is_atom(Flags, A),
    atom(Flags, A, M).

paren(Flags, A, 0) :-
    is_atom(Flags, A).

prec(Flags, A, 0) :-
    is_atom(Flags, A).

example :- example(k).

%
% Strings (non-italicized)
%
is_string(Flags, A) :-
    string(Flags, A, _).

string(_, A, mtext(A)) :-
    string(A).

ml(Flags, A, M) :-
    is_string(Flags, A),
    string(Flags, A, M).

paren(Flags, A, 0) :-
    is_string(Flags, A).

prec(Flags, A, 0) :-
    is_string(Flags, A).

example :- example("text").

%
% Parentheses: generic
%
ml(Flags, paren(A), M) :-
    paren(Flags, A, 1),
    !, ml(Flags, bracket(A), M).

ml(Flags, paren(A), M) :-
    paren(Flags, A, 2),
    !, ml(Flags, curly(A), M).

ml(Flags, paren(A), M) :-
    ml(Flags, parentheses(A), M).

paren(Flags, paren(A), P) :-
    paren(Flags, A, P0),
    P is P0 + 1.

prec(_, paren(_), 0).

% specific
ml(Flags, parentheses(A), mrow([mo('('), X, mo(')')])) :-
    ml(Flags, A, X).

paren(_, parentheses(_), 1).

prec(_, parentheses(_), 0).

ml(Flags, bracket(A), mrow([mo('['), X, mo(']')])) :-
    ml(Flags, A, X).

paren(_, bracket(_), 2).

prec(_, bracket(_), 0).

ml(Flags, curly(A), mrow([mo('{'), X, mo('}')])) :-
    ml(Flags, A, X).

paren(_, curly(_), 3).

prec(_, curly(_), 0).

ml(Flags, abs(A), mrow([mo('|'), X, mo('|')])) :-
    ml(Flags, A, X).

paren(Flags, abs(A), P) :-
    paren(Flags, A, P).

prec(Flags, abs(A), P) :-
    prec(Flags, parentheses(A), P).

example :- example(paren(x)).
example :- example(paren(bracket(x))).
example :- example(paren(paren(x))).
example :- example(paren(abs(x))).

%
% Lists (e.g., function arguments)
%
math(_, [H | T], list('', [H | T])).
math(_, (H, T), list(',', [H, T])).
math(_, (H; T), list(';', [H, T])).
math(_, (H| T), list('|', [H, T])).

ml(Flags, list(Sep, [H | T]), mrow([HX | TX])) :-
    ml(Flags, H, HX),
    ml(Flags, tail(Sep, T), TX).

ml(_, tail(_, []), []).

ml(Flags, tail(Sep, [H | T]), [SX, HX | TX]) :-
    ml(Flags, Sep, SX),
    ml(Flags, H, HX),
    ml(Flags, tail(Sep, T), TX).

paren(Flags, list(_, List), P) :-
    maplist({Flags}/[A, X] >> paren(Flags, A, X), List, PX),
    max_list(PX, P).

prec(_, list(Sep, _), P) :-
    current_op(Prec, _, Sep),
    !, P = Prec.

prec(_, list(_, _), 0).

example :- example([x, y, z]).
example :- example((x, y, z)).
example :- example((x ; y, z)).
example :- example((x | y, z)).
example :- example(paren([paren(x), paren(y)])).

%
% Colors
%
color(1, red, '#FFA0A0').
color(2, blue, '#A0A0FF').
color(3, green, '#A0FFA0').
color(4, '#FFFF00', '#DFDF80').
color(5, '#FF00FF', '#DF80DF').
color(6, '#00FFFF', '#DFDF80').

math(_, red(A), color(red, A)).
math(_, green(A), color(green, A)).
math(_, blue(A), color(blue, A)).
math(_, black(A), color(black, A)).
math(_, grey(A), color(grey, A)).
math(_, lightred(A), color('#FFA0A0', A)).

ml(Flags, color(Col, A), mstyle(mathcolor(Col), X)) :-
    ml(Flags, A, X).

paren(Flags, color(_, A), P) :-
    paren(Flags, A, P).

prec(Flags, color(_, A), P) :-
    prec(Flags, A, P).

%
% Other decorations
%
ml(Flags, overline(A), mover(accent(true), [X, mo(&(macr))])) :-
    ml(Flags, A, X).

paren(Flags, overline(A), P) :-
    paren(Flags, A, P).

prec(Flags, overline(A), P) :-
    prec(Flags, A, P).

% Proper average
math(_, mean(A), overline(A)).

% Underbrace with text
ml(
    Flags,
    underbrace(A, Under),
    munder([
        munder(accentunder(true), [X, mo(stretchy(true), &('UnderBrace'))]),
        Y])) :-
    ml(Flags, A, X),
    ml(Flags, Under, Y).

paren(Flags, underbrace(A, _), P) :-
    paren(Flags, A, P).

prec(Flags, underbrace(A, _), P) :-
    prec(Flags, A, P).

% red strike through
math(_, cancel(A), red(strike(black(A)))).

ml(Flags, strike(A), menclose(notation(updiagonalstrike), X)) :-
    ml(Flags, A, X).

paren(Flags, strike(A), P) :-
    paren(Flags, A, P).

prec(Flags, strike(A), P) :-
    prec(Flags, A, P).

% rounded box
ml(Flags, roundedbox(A), menclose(notation(roundedbox), X)) :-
    ml(Flags, A, X).

paren(Flags, roundedbox(A), P) :-
    paren(Flags, A, P).

prec(Flags, roundedbox(A), P) :-
    prec(Flags, A, P).

% invisible
ml(Flags, phantom(A), mphantom(X)) :-
    ml(Flags, A, X).

paren(Flags, phantom(A), P) :-
    paren(Flags, A, P).

prec(Flags, phantom(A), P) :-
    prec(Flags, A, P).

% formatting numbers
math(format_tratio(A), round2(A)).

math(format_pvalue(A), round2(A)).

math(quote(A), A).

example :- example(cancel('X')).
example :- example(paren([paren(red(x)), green(paren(y))])).
example :- example(underbrace(s, list('', ["instead of", ' ', sigma]))).

%
% Mistakes
%
ml(Flags, error(Err, A), M) :-
    select_option(error(_), Flags, New),
    ml([error(Err) | New], A, M).

paren(Flags, error(Err, A), P) :-
    select_option(error(_), Flags, New),
    paren([error(Err) | New], A, P).

prec(Flags, error(Err, A), P) :-
    select_option(error(_), Flags, New),
    prec([error(Err) | New], A, P).

% A instead of B
ml(Flags, instead_of(A, B), M) :-
    option(error(highlight), Flags, highlight),
    ml(Flags, underbrace(A, ["instead of", ' ', B]), M).

paren(Flags, instead_of(A, _), P) :-
    option(error(highlight), Flags, highlight),
    paren(Flags, A, P).

prec(Flags, instead_of(A, _), P) :-
    option(error(highlight), Flags, highlight),
    prec(Flags, A, P).

ml(Flags, instead_of(A, _), red(A)) :-
    option(error(show), Flags).

paren(Flags, instead_of(A, _), P) :-
    option(error(show), Flags),
    paren(Flags, A, P).

prec(Flags, instead_of(A, _), P) :-
    option(error(show), Flags),
    prec(Flags, A, P).

ml(Flags, instead_of(_, B), green(B)) :-
    option(error(fix), Flags).

paren(Flags, instead_of(_, B), P) :-
    option(error(show), Flags),
    paren(Flags, B, P).

prec(Flags, instead_of(_, B), P) :-
    option(error(show), Flags),
    prec(Flags, B, P).

% Left part omitted
ml(Flags, omit_left(quote(Expr)), M) :-
    option(error(highlight), Flags, highlight),
    compound(Expr),
    compound_name_arguments(Expr, Op, [L, R]),
    ml(Flags, [underbrace([L, Op], "omitted"), R], M).

ml(Flags, omit_left(quote(Expr)), M) :-
    option(error(show), Flags),
    compound(Expr),
    compound_name_arguments(Expr, Op, [L, R]),
    ml(Flags, [cancel([L, Op]), R], M).

ml(Flags, omit_left(quote(Expr)), M) :-
    option(error(fix), Flags),
    compound(Expr),
    compound_name_arguments(Expr, Op, [L, R]),
    ml(Flags, [green([L, Op]), R], M).

paren(Flags, omit_left(quote(Expr)), P) :-
    paren(Flags, Expr, P).

prec(Flags, omit_left(quote(Expr)), P) :-
    prec(Flags, Expr, P).

ml(Flags, omit_right(quote(Expr)), M) :-
    option(error(highlight), Flags, highlight),
    compound(Expr),
    compound_name_arguments(Expr, Op, [L, R]),
    ml(Flags, [L, underbrace([Op, R], "omitted")], M).

ml(Flags, omit_right(quote(Expr)), M) :-
    option(error(show), Flags),
    compound(Expr),
    compound_name_arguments(Expr, Op, [L, R]),
    ml(Flags, [L, cancel([Op, R])], M).

ml(Flags, omit_right(quote(Expr)), M) :-
    option(error(fix), Flags),
    compound(Expr),
    compound_name_arguments(Expr, Op, [L, R]),
    ml(Flags, [L, green([Op, R])], M).

paren(Flags, omit_right(quote(Expr)), P) :-
    paren(Flags, Expr, P).

prec(Flags, omit_right(quote(Expr)), P) :-
    prec(Flags, Expr, P).

ml(Flags, left_landed(Color, quote(Expr)), M) :-
    option(error(highlight), Flags, highlight),
    compound(Expr),
    compound_name_arguments(Expr, Op, [L, R]),
    color(Color, C, _),
    ml(Flags, [color(C, roundedbox(black([L, Op]))), R], M).

ml(Flags, left_landed(Color, quote(Expr)), M) :-
    option(error(show), Flags),
    compound(Expr),
    compound_name_arguments(Expr, Op, [L, R]),
    color(Color, C, _),
    ml(Flags, [color(C, roundedbox(black([L, Op]))), R], M).

ml(Flags, left_landed(_Color, quote(Expr)), M) :-
    option(error(fix), Flags),
    compound(Expr),
    compound_name_arguments(Expr, _, [_, R]),
    ml(Flags, R, M).

paren(Flags, left_landed(_, quote(Expr)), P) :-
    paren(Flags, Expr, P).

prec(Flags, left_landed(_, quote(Expr)), P) :-
    prec(Flags, Expr, P).

ml(Flags, right_landed(Color, quote(Expr)), M) :-
    option(error(highlight), Flags, highlight),
    compound(Expr),
    compound_name_arguments(Expr, Op, [L, R]),
    color(Color, C, _),
    ml(Flags, [L, color(C, roundedbox(black([Op, R])))], M).

ml(Flags, right_landed(Color, quote(Expr)), M) :-
    option(error(show), Flags),
    compound(Expr),
    compound_name_arguments(Expr, Op, [L, R]),
    color(Color, C, _),
    ml(Flags, [L, color(C, roundedbox(black([Op, R])))], M).

ml(Flags, right_landed(_Color, quote(Expr)), M) :-
    option(error(fix), Flags),
    compound(Expr),
    compound_name_arguments(Expr, _Op, [L, _R]),
    ml(Flags, L, M).

paren(Flags, right_landed(_, quote(Expr)), P) :-
    paren(Flags, Expr, P).

prec(Flags, right_landed(_, quote(Expr)), P) :-
    prec(Flags, Expr, P).

ml(Flags, left_elsewhere(Color, quote(Expr)), M) :-
    option(error(highlight), Flags, highlight),
    compound(Expr),
    compound_name_arguments(Expr, Op, [L, R]),
    color(Color, _, C),
    ml(Flags, [color(C, roundedbox(phantom([L, Op]))), R], M).

ml(Flags, left_elsewhere(_Color, quote(Expr)), M) :-
    option(error(show), Flags),
    compound(Expr),
    compound_name_arguments(Expr, _Op, [_L, R]),
    ml(Flags, R, M).

ml(Flags, left_elsewhere(Color, quote(Expr)), M) :-
    option(error(fix), Flags),
    compound(Expr),
    compound_name_arguments(Expr, Op, [L, R]),
    color(Color, C, _),
    ml(Flags, [color(C, [L, Op]), R], M).

paren(Flags, left_elsewhere(_, quote(Expr)), P) :-
    paren(Flags, Expr, P).

prec(Flags, left_elsewhere(_, quote(Expr)), P) :-
    prec(Flags, Expr, P).

ml(Flags, right_elsewhere(Color, quote(Expr)), M) :-
    option(error(highlight), Flags, highlight),
    compound(Expr),
    compound_name_arguments(Expr, Op, [L, R]),
    color(Color, _, C),
    ml(Flags, [L, color(C, roundedbox(phantom([Op, R])))], M).

ml(Flags, right_elsewhere(_Color, quote(Expr)), M) :-
    option(error(show), Flags),
    compound(Expr),
    compound_name_arguments(Expr, _Op, [L, _R]),
    ml(Flags, L, M).

ml(Flags, right_elsewhere(Color, quote(Expr)), M) :-
    option(error(fix), Flags),
    compound(Expr),
    compound_name_arguments(Expr, Op, [L, R]),
    color(Color, C, _),
    ml(Flags, [L, color(C, [Op, R])], M).

paren(Flags, right_elsewhere(_, quote(Expr)), P) :-
        paren(Flags, Expr, P).

prec(Flags, right_elsewhere(_, quote(Expr)), P) -->
        prec(Flags, Expr, P).

math(_, expert(A = B), A -> B).
math(_, buggy(A \= B, _), A ~> B).

example :- example([error(highlight)], instead_of(sigma, s)).
example :- example([error(fix)], instead_of(sigma, s)).
example :- example([error(show)], instead_of(sigma, s)).

%
% Abbreviations
%
math(_, denoting(A, _, _), A).

example :- example(denoting(s, t + u, "something")).
example :- example(a + denoting(s, t + u, "something")).

% Collect abbreviations
abbreviations(Flags, A, W) :-
    denot(Flags, A, X),
    list_to_set(X, W).

denot(_, A, []) :-
    atomic(A).

denot(Flags, denoting(Abbrev, Expr, Desc), W) :-
    !, denot(Flags, Expr, T),
    W = [denoting(Abbrev, Expr, Desc) | T].

% See compound below
%
% denot(Flags, instead_of(A, B), W) :-
%     option(error(highlight), Flags, highlight),
%     !, denot(Flags, A, X),
%     denot(Flags, B, Y),
%     append(X, Y, W).

denot(Flags, instead_of(A, _), W) :-
    option(error(show), Flags),
    !, denot(Flags, A, W).

denot(Flags, instead_of(_, B), W) :-
    option(error(fix), Flags),
    !, denot(Flags, B, W).

denot(Flags, Comp, With) :-
    compound(Comp),
    compound_name_arguments(Comp, _, Args),
    maplist({Flags}/[A, W] >> denot(Flags, A, W), Args, List),
    append(List, With).

% Render abbreviations
denoting(Flags, A, Empty) :-
    abbreviations(Flags, A, []),
    !, Empty = [].

denoting(Flags, A, [M]) :-
    abbreviations(Flags, A, [denoting(X, Exp, Des)]),
    !,
    ml(Flags, [' ', "with", ' ', X = Exp, ' ', "denoting", ' ', Des, "."], M).

denoting(Flags, A, [M | MT]) :-
    abbreviations(Flags, A, [denoting(X, Exp, Des) | T]),
    !,
    ml(Flags, ["with", ' ', X = Exp, ' ', "denoting", ' ', Des, ",", ' '], M),
    and(Flags, T, MT).

and(Flags, [], [X]) :-
    ml(Flags, ".", X).

and(Flags, [denoting(X, Exp, Des) | T], [M | MT]) :-
    ml(Flags, ["and", ' ', X = Exp, ' ', "denoting", ' ', Des], M),
    and(Flags, T, MT).

%
% Operators
%
ml(Flags, '100%'(A), X) :-
    select_option(mult(M), Flags, New, 1),
    _100M is 100 * M,
    ml([mult(_100M) | New], A '%', X).

math(_, A '_' B ^ C, subsup(A, B, C)).

ml(Flags, subsup(A, B, C), msubsup([X, Y, Z])) :-
    prec(Flags, subsup(A, B, C), P),
    prec(Flags, A, Inner),
    ( P =< Inner
      -> ml(Flags, paren(A), X)
      ; ml(Flags, A, X)
    ), ml(Flags, B, Y),
    ml(Flags, C, Z).

paren(Flags, subsup(A, _, _), P) :-
    paren(Flags, A, P).

prec(Flags, subsup(X, _, Z), P) :-
    prec(Flags, X^Z, P).

math(_, A '_' B, sub(A, B)).

ml(Flags, sub(A, B), msub([X, Y])) :-
    prec(Flags, sub(A, B), P),
    prec(Flags, A, Inner),
    ( P =< Inner
      -> ml(Flags, paren(A), X)
      ; ml(Flags, A, X)
    ), ml(Flags, B, Y).

paren(Flags, sub(A, _), P) :-
    paren(Flags, A, P).

prec(Flags, sub(A, _), P) :-
    prec(Flags, A, P).

math(_, A^B, sup(A, B)).

% experimental: sin^2 x for "simple" x
ml(Flags, sup(Sin, X), M) :-
    prec(Flags, Sin, P),
    prec(Flags, sin(x), P),
    paren(Flags, X, 0),
    prec(Flags, X, 0),
    !, ml([replace(Sin^X, Sin)], Sin, M).

ml(Flags, sup(A, B), msup([X, Y])) :-
    prec(Flags, sup(A, B), P),
    prec(Flags, A, Inner),
    ( P =< Inner
      -> ml(Flags, paren(A), X)
      ; ml(Flags, A, X)
    ), ml(Flags, B, Y).

paren(Flags, sup(A, _), P) :-
    paren(Flags, A, P).

prec(_, sup(_, _), P) :-
    current_op(Prec, xfy, ^),
    P = Prec.

% Omit multiplication sign in "simple" products
math(Flags, A * B, M) :-
    paren(Flags, A / x, 0),
    !, M = A invisible_times B.

% Negative sign has same precedence as binary minus
math(Flags, -A, operator(P, fx, -, A)) :-
    prec(Flags, a-b, P).

% Prefix and postfix operators (e.g., factorial)
math(_, Comp, operator(Prec, Fix, Op, A)) :-
    compound(Comp),
    compound_name_arguments(Comp, Op, [A]),
    current_op(P, Fix, Op), Prec = P,
    member(Fix, [xf, yf, fx, fy]).

ml(Flags, operator(P, fx, Op, A), mrow([F, X])) :-
    prec(Flags, A, Inner),
    ( P =< Inner
      -> ml(Flags, paren(A), X)
      ; ml(Flags, A, X)
    ), ml(Flags, Op, F).

ml(Flags, operator(P, fy, Op, A), mrow([F, Y])) :-
    prec(Flags, A, Inner),
    ( P < Inner
      -> ml(Flags, paren(A), Y)
      ; ml(Flags, A, Y)
    ), ml(Flags, Op, F).

ml(Flags, operator(P, xf, Op, A), mrow([X, F])) :-
    prec(Flags, A, Inner),
    ( P =< Inner
      -> ml(Flags, paren(A), X)
      ; ml(Flags, A, X)
    ), ml(Flags, Op, F).

ml(Flags, operator(P, yf, Op, A), mrow([Y, F])) :-
    prec(Flags, A, Inner),
    ( P < Inner
      -> ml(Flags, paren(A), Y)
      ; ml(Flags, A, Y)
    ), ml(Flags, Op, F).

paren(Flags, operator(Prec, Fix, _, A), Paren) :-
    member(Fix, [xf, fx]),
    paren(Flags, A, P),
    prec(Flags, A, Inner),
    ( Prec =< Inner
      -> Paren is P + 1
      ; Paren = P
    ).

paren(Flags, operator(Prec, Fix, _, A), Paren) :-
    member(Fix, [yf, fy]),
    paren(Flags, A, P),
    prec(Flags, A, Inner),
    ( Prec < Inner
      -> Paren is P + 1
      ; Paren = P
    ).

prec(_, operator(P, _, _, _), P).

math(_, Comp, operator(Prec, Fix, Op, A, B)) :-
    compound(Comp),
    compound_name_arguments(Comp, Op, [A, B]),
    current_op(P, Fix, Op), Prec = P,
    member(Fix, [xfx, yfx, xfy]).

ml(Flags, operator(P, xfx, Op, A, B), mrow([X, F, Y])) :-
    prec(Flags, A, PrecA),
    ( P =< PrecA
      -> ml(Flags, paren(A), X)
      ; ml(Flags, A, X)
    ), prec(Flags, B, PrecB),
    ( P =< PrecB
      -> ml(Flags, paren(B), Y)
      ; ml(Flags, B, Y)
    ), ml(Flags, Op, F).

ml(Flags, operator(P, xfy, Op, A, B), mrow([X, F, Y])) :-
    prec(Flags, A, PrecA),
    ( P =< PrecA
      -> ml(Flags, paren(A), X)
      ; ml(Flags, A, X)
    ), prec(Flags, B, PrecB),
    ( P < PrecB
      -> ml(Flags, paren(B), Y)
      ; ml(Flags, B, Y)
    ), ml(Flags, Op, F).

ml(Flags, operator(P, yfx, Op, A, B), mrow([Y, F, X])) :-
    prec(Flags, A, PrecA),
    ( P < PrecA
      -> ml(Flags, paren(A), Y)
      ; ml(Flags, A, Y)
    ), prec(Flags, B, PrecB),
    ( P =< PrecB
      -> ml(Flags, paren(B), X)
      ; ml(Flags, B, X)
    ), ml(Flags, Op, F).

paren(Flags, operator(Prec, xfx, _, A, B), P) :-
    paren(Flags, A, PA),
    prec(Flags, A, PrecA),
    ( Prec =< PrecA
      -> ParenA is PA + 1
      ; ParenA = PA
    ), paren(Flags, B, PB),
    prec(Flags, B, PrecB),
    ( Prec =< PrecB
      -> ParenB is PB + 1
      ; ParenB = PB
    ), P is max(ParenA, ParenB).

paren(Flags, operator(Prec, yfx, _, A, B), P) :-
    paren(Flags, A, PA),
    prec(Flags, A, PrecA),
    ( Prec < PrecA
      -> ParenA is PA + 1
      ; ParenA = PA
    ), paren(Flags, B, PB),
    prec(Flags, B, PrecB),
    ( Prec =< PrecB
      -> ParenB is PB + 1
      ; ParenB = PB
    ), P is max(ParenA, ParenB).

paren(Flags, operator(Prec, xfy, _, A, B), P) :-
    paren(Flags, A, PA),
    prec(Flags, A, PrecA),
    ( Prec =< PrecA
      -> ParenA is PA + 1
      ; ParenA = PA
    ), paren(Flags, B, PB),
    prec(Flags, B, PrecB),
    ( Prec < PrecB
      -> ParenB is PB + 1
      ; ParenB = PB
    ), P is max(ParenA, ParenB).

prec(_, operator(P, _, _, _, _), P).

example :- example(a^3 + 3*a^2*b + 3*a*b^2 + b^3).
example :- example(a^b).
example :- example((s!)!).
example :- example(a + b + c).
example :- example(a + (b + c)).
example :- example(a - b - c).
example :- example(a - (b - c)).
example :- example((a + b) * (a - b) = a^two - b^two).

%
% Numbers
%
is_positive(Flags, A) :-
    positive(Flags, A, _).

positive(Flags, A, X) :-
    number(A), A >= 0,
    select_option(mult(M), Flags, New),
    !, AM is A*M,
    ml(New, AM, X).

positive(Flags, A, mn(N)) :-
    number(A),
    A >= 0,
    option(round(3), Flags),
    !, format(atom(N), '~3f', A).

positive(Flags, A, mn(N)) :-
    number(A),
    A >= 0,
    option(round(2), Flags),
    !, format(atom(N), '~2f', A).

positive(Flags, A, mn(N)) :-
    number(A),
    A >= 0,
    option(round(1), Flags),
    !, format(atom(N), '~1f', A).

positive(Flags, A, mn(N)) :-
    number(A),
    A >= 0,
    option(round(0), Flags),
    !, format(atom(N), '~0f', A).

positive(Flags, A, mn(N)) :-
    number(A),
    A >= 0,
    option(round(g), Flags),
    !, format(atom(N), '~g', A).

positive(_, A, mn(A)) :-
    number(A),
    A >= 0.

positive(Flags, A, X) :-
    string(A),
    number_string(N, A),
    positive(Flags, N, X).

ml(Flags, A, M) :-
    is_positive(Flags, A),
    positive(Flags, A, M).

paren(Flags, A, 0) :-
    is_positive(Flags, A).

prec(Flags, A, 0) :-
    is_positive(Flags, A).

is_negative(Flags, A) :-
    negative(Flags, A, _).

negative(Flags, A, X) :-
    number(A),
    A < 0,
    P is abs(A),
    ml(Flags, -P, X).

negative(Flags, A, X) :-
    string(A),
    number_string(N, A),
    negative(Flags, N, X).

ml(Flags, A, M) :-
    is_negative(Flags, A),
    negative(Flags, A, M).

paren(Flags, A, 0) :-
    is_negative(Flags, A).

prec(Flags, A, P) :-
    is_negative(Flags, A),
    prec(Flags, -A, P).

% Force rendering string as number
ml(_, number(A), M) :-
    number_string(N, A),
    N >= 0,
    !, M = mn(A).

ml(Flags, number(A), X) :-
    quantity(A, N, Options),
    N < 0,
    Abs is abs(N),
    quantity(AX, Abs, Options),
    ml(Flags, [-, number(AX)], X).

paren(Flags, number(A), P) :-
    term_string(T, A),
    paren(Flags, T, P).

prec(Flags, number(A), P) :-
    term_string(T, A),
    prec(Flags, T, P).

ml(Flags, round(A), M) :-
    ml([round(g) | Flags], A, M).

paren(Flags, round(A), P) :-
    paren(Flags, A, P).

prec(Flags, round(A), P) :-
    prec(Flags, A, P).

ml(Flags, round0(A), M) :-
    ml([round(0) | Flags], A, M).

paren(Flags, round0(A), P) :-
    paren(Flags, A, P).

prec(Flags, round0(A), P) :-
    prec(Flags, A, P).

ml(Flags, round1(A), M) :-
    ml([round(1) | Flags], A, M).

paren(Flags, round1(A), P) :-
    paren(Flags, A, P).

prec(Flags, round1(A), P) :-
    prec(Flags, A, P).

ml(Flags, round2(A), M) :-
    ml([round(2) | Flags], A, M).

paren(Flags, round2(A), P) :-
    paren(Flags, A, P).

prec(Flags, round2(A), P) :-
    prec(Flags, A, P).

ml(Flags, round3(A), M) :-
    ml([round(3) | Flags], A, M).

paren(Flags, round3(A), P) :-
    paren(Flags, A, P).

prec(Flags, round3(A), P) :-
    prec(Flags, A, P).

example :- example(5^2).
example :- example((-5)^(-2)).
example :- example((5!)!).
example :- example(1 + 2 + (-3)).
example :- example(1 + (-2 + 3)).
example :- example(1 - (-2) - 3).
example :- example(1 - ((-2) - 3)).
example :- example((a + b) * (a - b) = a^2 - b^2).

%
% Units
%
is_unit(Flags, A) :-
    unit(Flags, A, _).

unit(_, A, M) :-
    unit(A, M).

unit('%', "%").
unit('kg', "kg").

ml(Flags, A, mrow([X, Sp, Unit])) :-
    compound(A),
    compound_name_arguments(A, U, [Num]),
    is_unit(Flags, U),
    unit(Flags, U, Unit),
    ml(Flags, Num, X),
    ml(Flags, '_', Sp).

example :- example(10 '%').
example :- example('100%'(0.1)).
example :- example(kg(5)).

%
% Fractions
%
ml(Flags, frac(A, B), mfrac([X, Y])) :-
    ml(Flags, A, X),
    ml(Flags, B, Y).

paren(_, frac(_, _), 0).

prec(Flags, frac(A, B), P) :-
    prec(Flags, A/B, P).

% Large fraction
ml(Flags, dfrac(A, B), mstyle(displaystyle(true), X)) :-
    ml(Flags, frac(A, B), X).

paren(Flags, dfrac(A, B), P) :-
    paren(Flags, frac(A, B), P).

prec(Flags, dfrac(A, B), P) :-
    prec(Flags, frac(A, B), P).

ml(Flags, choose(A, B),
        mrow([mo('('), mfrac(linethickness(0), [X, Y]), mo(')')])) :-
    ml(Flags, A, X),
    ml(Flags, B, Y).

paren(_, choose(_, _), 1).

prec(Flags, choose(A, B), P) :-
    prec(Flags, A^B, P).

ml(Flags, dchoose(A, B),
       mrow([mo('('), mfrac(mstyle([displaystyle(true), linethickness(0)]),
                            [X, Y]), mo(')')])) :-
    ml(Flags, A, X),
    ml(Flags, B, Y).

paren(Flags, dchoose(A, B), P) :-
    paren(Flags, choose(A, B), P).

prec(Flags, dchoose(A, B), P) :-
    prec(Flags, choose(A, B), P).

example :- example(frac(1.5, 2)^2).
example :- example(frac(small, small) = dfrac(large, large)).

%
% Trigonometric functions
%
math(_, sin(A), trig(sin, A)).
math(_, cos(A), trig(cos, A)).
math(_, tan(A), trig(tan, A)).

ml(Flags, trig(Fun, Arg), M) :-
    paren(Flags, Arg, 0),
    prec(Flags, Arg, 0),
    !, ( select_option(replace(Sin^N, Sin), Flags, New)
      -> ml(New, Fun^N, F)
      ; ml(Flags, Fun, F)
    ), ml(Flags, Arg, X),
    M = mrow([F, mo(&(af)), X]).

ml(Flags, trig(Fun, Arg), mrow([F, mo(&(af)), X])) :-
    ( select_option(replace(Sin^N, Sin), Flags, New)
      -> ml(New, Fun^N, F)
      ; ml(Flags, Fun, F)
    ), ml(Flags, paren(Arg), X).

paren(Flags, trig(_, Arg), P) :-
    prec(Flags, Arg, 0),
    !, paren(Flags, Arg, P).

paren(Flags, trig(_, Arg), P) :-
    paren(Flags, Arg, Paren),
    P is Paren + 1.

prec(Flags, trig(Fun, Arg), P) :-
    prec(Flags, Fun*Arg, Prec),
    P is Prec - 1.

example :- example(sin(pi)).
example :- example(sin(pi/2)).
example :- example(sin(pi)^2).
example :- example(sin(pi)*cos(pi)).
example :- example(sin(a + b)).
example :- example(sin(a!)^2).

%
% Special functions
%
math(_, 'TTEST'(D, T0, EOT, Mu, S, S_T0, S_EOT, N),
     fun('TTEST', (D, T0, EOT, Mu, S, S_T0, S_EOT, N))).

math(_, tratio(X, Mu, S, N), fun("paired t-test", (X, S; Mu, N))).

ml(Flags, sum(I, From, To, A), mrow([munderover([\['&sum;'], XFrom, XTo]), X])) :-
    ml(Flags, I = From, XFrom),
    ml(Flags, To, XTo),
    ml(Flags, A, X).

paren(Flags, sum(_, _, _, A), P) :-
    paren(Flags, A, P).

prec(Flags, sum(_, _, _, _), P) :-
    prec(Flags, x+y, P).

ml(Flags, argmin(I, _, _, A), M) :-
    ml(Flags, fun(under("argmin", I), A), M).

paren(Flags, argmin(_, _, _, A), Paren) :-
    paren(Flags, A, Paren).

prec(Flags, argmin(_, _, _, _), Prec) :-
    prec(Flags, x+y, Prec).

ml(Flags, argmax(I, _, _, A), M) :-
    ml(Flags, fun(under("argmax", I), A), M).

paren(Flags, argmax(_, _, _, A), Paren) :-
    paren(Flags, A, Paren).

prec(Flags, argmax(_, _, _, _), Prec) :-
    prec(Flags, x+y, Prec).

ml(Flags, under(A, B), munder([X, Y])) :-
    ml(Flags, A, X),
    ml(Flags, B, Y).

paren(Flags, under(A, _), Paren) :-
    paren(Flags, A, Paren).

prec(Flags, under(A, _), Prec) :-
    prec(Flags, A, Prec).

ml(Flags, sqrt(A), msqrt(X)) :-
    ml(Flags, A, X).

paren(_, sqrt(_), 0).

prec(Flags, sqrt(_), Prec) :-
    prec(Flags, x^y, Prec).

% t-distribution
math(_, tt(T, DF), fun('P', (abs('T') >= T ; "df" = DF))).
math(_, ut(T, DF), fun('P', ('T' >= T ; "df" = DF))).
math(_, pt(T, DF), fun('P', ('T' =< T ; "df" = DF))).

math(_, instead_of(pt(PT, DF), tt(TT, DF)),
    fun('P', (instead_of('T' =< PT, abs('T') >= TT) ; "df" = DF))).

math(_, instead_of(ut(UT, DF), tt(TT, DF)),
    fun('P', (instead_of('T' >= UT, abs('T') >= TT) ; "df" = DF))).

math(_, dbinom(K, N, P), fun('P' '_' "Bi", ['X' = K ; (N, P)])).
math(_, pbinom(K, N, P), fun('P' '_' "Bi", ['X' =< K ; (N, P)])).
math(_, ubinom(K, N, P), fun('P' '_' "Bi", ['X' >= K ; (N, P)])).
math(_, qbinom(Alpha, N, P), fun('Q' '_' "Bi", [Alpha ; (N, P)])).
math(_, uqbinom(Alpha, N, P), fun('Q' '_' "Bi", [1 - Alpha ; (N, P)])).

% Bit unusual terminology
math(_, bernoulli(Succ, N, Pi), successes(Succ, Pi) * failures(N-Succ, Pi)).
math(_, successes(Succ, Pi), Pi^Succ).
math(_, failures(Fail, Pi), (1-Pi)^Fail).

% General functions
ml(Flags, fun(Name, Args), mrow([N, &(af), A])) :-
    ml(Flags, Name, N),
    ml(Flags, paren(Args), A).

paren(Flags, fun(_, Args), Paren) :-
    paren(Flags, paren(Args), Paren).

prec(Flags, fun(_, _), Prec) :-
    prec(Flags, x^y, Prec).

example :- example(sqrt(2)).
example :- example(sqrt(2)^2).
example :- example(dbinom(k, n, pi)).
example :- example(pbinom(k, n, pi)).
example :- example(ubinom(k, n, pi)).
example :- example(qbinom(alpha, n, pi)).
example :- example(uqbinom(alpha, n, pi)).
example :- example(dbinom(k, n, pi) =
                   choose(n, k) * successes(n, pi) * failures(n-k, pi)).
example :- example(pbinom(k, 'N', pi) =
                   sum(i, k, 'N', choose('N', k) * dbinom(i, 'N', k))).
example :- example(sum(i, k, 'N', i) + sum(i, k, 'N', i)).





