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
% Interface
%
mml(A) -->
    mml([highlight(all)], A).

mml(Flags, A) -->
    { ml(Flags, A, M) },
    html(math(M)).

mathml(A, M) :-
    mathml([highlight(all)], A, M).

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
% Hook for custom definitions
%
% e.g., to replace s_D by sub(s, 'D'), use math_hook(_, s_D, sub(s, 'D')).
%
:- multifile ml_hook/3.
:- multifile math_hook/3.

ml(Flags, A, M) :-
    ml_hook(Flags, A, M),
    !.

math(Flags, A, M) :-
    math_hook(Flags, A, M),
    !.

%
% Macros (e.g., red(X) for color(red, X)
%
ml(Flags, A, M) :-
    math(Flags, A, X),
    !, ml(Flags, X, M).

paren(Flags, A, P) :-
    math(Flags, A, X),
    !, paren(Flags, X, P).

%
% Precedence
%
% Macros
precedence(Flags, A, P) :-
    math(Flags, A, X),
    !, precedence(Flags, X, P).

% Explicit definitions
precedence(Flags, A, P) :-
    prec(Flags, A, Prec),
    !, P = Prec.

% Compounds
precedence(Flags, A, fun-P) :-
    compound(A),
    !, compound_name_arguments(A, _, Args),
    maplist({Flags}/[AX, F, PX] >> precedence(Flags, AX, F-PX), Args, _, Precs),
    max_list(Precs, P).

% Otherwise, zero
precedence(_, _, atomic-0).

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

example :- example(alpha).

%
% General atoms
%
is_atom(Flags, A) :-
    atom(Flags, A, _),
    \+ is_identifier(Flags, A),
    \+ is_punctuation(Flags, A),
    \+ is_operator(Flags, A).

atom(_, A, mi(A)) :-
    atom(A).

ml(Flags, A, M) :-
    is_atom(Flags, A),
    atom(Flags, A, M).

paren(Flags, A, 0) :-
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

prec(_, paren(_), paren-0).

% specific
ml(Flags, parentheses(A), mrow([mo('('), X, mo(')')])) :-
    ml(Flags, A, X).

paren(_, parentheses(_), 1).

prec(_, parentheses(_), paren-0).

ml(Flags, bracket(A), mrow([mo('['), X, mo(']')])) :-
    ml(Flags, A, X).

paren(_, bracket(_), 2).

prec(_, bracket(_), paren-0).

ml(Flags, curly(A), mrow([mo('{'), X, mo('}')])) :-
    ml(Flags, A, X).

paren(_, curly(_), 3).

prec(_, curly(_), paren-0).

ml(Flags, abs(A), mrow([mo('|'), X, mo('|')])) :-
    ml(Flags, A, X).

paren(Flags, abs(A), P) :-
    paren(Flags, A, P).

prec(_, abs(_), paren-0).

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
    !, P = list-Prec.

prec(_, list(_, _), list-0).

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

color(Flags, Error, Color, Flags) :-
    member(color(Error, Color), Flags),
    !.

color(Flags, Error, Color, [color(Error, Color) | Flags]) :-
    findall(C, member(color(_, C), Flags), List),
    length(List, Len),
    Index is Len mod 6 + 1,
    color(Index, Color, _).

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
    precedence(Flags, A, P).

%
% Other decorations
%
ml(Flags, overline(A), mover(accent(true), [X, mo(&(macr))])) :-
    ml(Flags, A, X).

paren(Flags, overline(A), P) :-
    paren(Flags, A, P).

% Put average(x)^2 in parentheses
prec(Flags, overline(A), accent-P) :-
    precedence(Flags, a*A, _-P).

% Proper average
math(_, mean(A), overline(A)).

% Underbrace with text
ml(Flags, underbrace(A, Under),
    munder([munder(accentunder(true), [X, mo(stretchy(true), &('UnderBrace'))]), Y])) :-
    ml(Flags, A, X),
    ml(Flags, Under, Y).

paren(Flags, underbrace(A, _), P) :-
    paren(Flags, A, P).

prec(Flags, underbrace(A, _), P) :-
    precedence(Flags, A, P).

% Strike through
math(_, cancel(Color, A), color(Color, strike(black(A)))).

ml(Flags, strike(A), menclose(notation(updiagonalstrike), X)) :-
    ml(Flags, A, X).

paren(Flags, strike(A), P) :-
    paren(Flags, A, P).

% rounded box
ml(Flags, roundedbox(A), menclose(notation(roundedbox), X)) :-
    ml(Flags, A, X).

paren(Flags, roundedbox(A), P) :-
    paren(Flags, A, P).

% Colored or Box, depending on nested error
ml(Flags, color_or_box(Col, A), X) :-
    erroneous(Flags, A, [_ | _]),
    !, ml(Flags, color(Col, roundedbox(color(black, A))), X).
    
ml(Flags, color_or_box(Col, A), X) :-
    ml(Flags, color(Col, A), X).
    
paren(Flags, color_or_box(_, A), P) :-
    paren(Flags, A, P).

prec(Flags, color_or_box(_, A), P) :-
    precedence(Flags, A, P).

% invisible
ml(Flags, phantom(A), mphantom(X)) :-
    ml(Flags, A, X).

paren(Flags, phantom(A), P) :-
    paren(Flags, A, P).

% formatting numbers
math(_, format_tratio(A), round2(A)).

math(_, format_pvalue(A), round2(A)).

math(_, quote(A), A).

example :- example(cancel(red, 'X')).
example :- example(paren([paren(red(x)), green(paren(y))])).
example :- example(underbrace(s, list('', ["instead of", ' ', sigma]))).

%
% Mistakes
%
erroneous(buggy(Err, A), Errors) :-
    !, erroneous(A, T),
    Errors = [Err | T].

erroneous(instead_of(Err, _A, Instead, Of), Errors) :-
    !, erroneous(Instead, I),
    erroneous(Of, I),
    append([[Err], Instead, Of], Errors).

erroneous(A, Errors) :-
    compound(A),
    !, compound_name_arguments(A, _, Args),
    maplist(erroneous, Args, Errs),
    append(Errs, Errors).

erroneous(_, []).

highlight(Flags, Err) :-
    member(highlight(Err), Flags).

highlight(Flags, _) :-
    member(highlight(all), Flags).

show(Flags, Err) :-
    member(show(Err), Flags).

show(Flags, _) :-
    member(show(all), Flags).

fix(Flags, Err) :-
    member(fix(Err), Flags).

fix(Flags, _) :-
    member(fix(all), Flags).

math(_, expert(quote(_Feedback), A), A).
math(_, buggy(quote(_Feedback), A), A).

paren(Flags, error(Err, Mode, A), P) :-
    C =.. [Err, Mode],
    paren([C | Flags], A, P).

prec(Flags, error(Err, Mode, A), P) :-
    C =.. [Err, Mode],
    precedence([C | Flags], A, P).

ml(Flags, error(Err, Mode, A), M) :-
    C =.. [Err, Mode],
    ml([C | Flags], A, M).

paren(Flags, error(Err, Mode, A), P) :-
    C =.. [Err, Mode],
    paren([C | Flags], A, P).

prec(Flags, error(Err, Mode, A), P) :-
    C =.. [Err, Mode],
    precedence([C | Flags], A, P).

% A instead of B
ml(Flags, instead_of(Err, Instead, Instead, Of), M) :-
    highlight(Flags, Err),
    !, ml(Flags, underbrace(Instead, ["instead of", ' ', Of]), M).

ml(Flags, instead_of(Err, A, Instead, Of), M) :-
    highlight(Flags, Err),
    ml(Flags, underbrace(A, [Instead, ' ', "instead of", ' ', Of]), M).

paren(Flags, instead_of(Err, A, _, _), P) :-
    highlight(Flags, Err),
    paren(Flags, A, P).

prec(Flags, instead_of(Err, A, _, _), P) :-
    highlight(Flags, Err),
    precedence(Flags, A, P).

ml(Flags, instead_of(Err, A, _, _), M) :-
    show(Flags, Err),
    color(Flags, Err, Color, New),
    ml(New, color_or_box(Color, A), M).

paren(Flags, instead_of(Err, A, _, _), P) :-
    show(Flags, Err),
    paren(Flags, A, P).

prec(Flags, instead_of(Err, A, _, _), P) :-
    show(Flags, Err),
    precedence(Flags, A, P).

ml(Flags, instead_of(Err, _, _, Of), M) :-
    fix(Flags, Err),
    color(Flags, Err, Color, New),
    ml(New, color(Color, Of), M).

paren(Flags, instead_of(Err, _, _, Of), P) :-
    fix(Flags, Err),
    paren(Flags, Of, P).

prec(Flags, instead_of(Err, _, _, Of), P) :-
    fix(Flags, Err),
    precedence(Flags, Of, P).

% Left part omitted
ml(Flags, omit_left(Err, quote(Expr)), M) :-
    highlight(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    ml(Flags, [underbrace([L, Op], "omitted"), R], M).

ml(Flags, omit_left(Err, quote(Expr)), M) :-
    show(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    color(Flags, Err, Color, New),
    ml(New, [cancel(Color, [L, Op]), R], M).

ml(Flags, omit_left(Err, quote(Expr)), M) :-
    fix(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    color(Flags, Err, Color, New),
    ml(New, [color(Color, [L, Op]), R], M).

paren(Flags, omit_left(_Err, quote(Expr)), P) :-
    paren(Flags, Expr, P).

ml(Flags, omit_right(Err, quote(Expr)), M) :-
    highlight(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    ml(Flags, [L, underbrace([Op, R], "omitted")], M).

ml(Flags, omit_right(Err, quote(Expr)), M) :-
    show(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    color(Flags, Err, Color, New),
    ml(New, [L, cancel(Color, [Op, R])], M).

ml(Flags, omit_right(Err, quote(Expr)), M) :-
    fix(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    color(Flags, Err, Color, New),
    ml(New, [L, color(Color, [Op, R])], M).

paren(Flags, omit_right(_Err, quote(Expr)), P) :-
    paren(Flags, Expr, P).

ml(Flags, left_landed(Err, quote(Expr)), M) :-
    highlight(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    color(Flags, Err, Color, New),
    ml(New, [color(Color, roundedbox(black([L, Op]))), R], M).

ml(Flags, left_landed(Err, quote(Expr)), M) :-
    show(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    color(Flags, Err, Color, New),
    ml(New, [color_or_box(Color, [L, Op]), R], M).

ml(Flags, left_landed(Err, quote(Expr)), M) :-
    fix(Flags, Err),
    compound_name_arguments(Expr, _, [_, R]),
    color(Flags, Err, _, New),
    ml(New, R, M).

paren(Flags, left_landed(_Err, quote(Expr)), P) :-
    paren(Flags, Expr, P).

ml(Flags, right_landed(Err, quote(Expr)), M) :-
    highlight(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    color(Flags, Err, Color, New),
    ml(New, [L, color(Color, roundedbox(black([Op, R])))], M).

ml(Flags, right_landed(Err, quote(Expr)), M) :-
    show(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    color(Flags, Err, Color, New),
    ml(New, [L, color_or_box(Color, [Op, R])], M).

ml(Flags, right_landed(Err, quote(Expr)), M) :-
    fix(Flags, Err),
    compound_name_arguments(Expr, _Op, [L, _R]),
    color(Flags, Err, _, New),
    ml(New, L, M).

paren(Flags, right_landed(_Err, quote(Expr)), P) :-
    paren(Flags, Expr, P).

ml(Flags, left_elsewhere(Err, quote(Expr)), M) :-
    highlight(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    color(Flags, Err, Color, New),
    ml(New, [color(Color, roundedbox(phantom([L, Op]))), R], M).

ml(Flags, left_elsewhere(Err, quote(Expr)), M) :-
    show(Flags, Err),
    compound_name_arguments(Expr, _Op, [_L, R]),
    color(Flags, Err, _, New),
    ml(New, R, M).

ml(Flags, left_elsewhere(Err, quote(Expr)), M) :-
    fix(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    color(Flags, Err, Color, New),
    ml(New, [color_or_box(Color, [L, Op]), R], M).

paren(Flags, left_elsewhere(_Err, quote(Expr)), P) :-
    paren(Flags, Expr, P).

ml(Flags, right_elsewhere(Err, quote(Expr)), M) :-
    highlight(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    color(Flags, Err, Color, New),
    ml(New, [L, color(Color, roundedbox(phantom([Op, R])))], M).

ml(Flags, right_elsewhere(Err, quote(Expr)), M) :-
    show(Flags, Err),
    compound_name_arguments(Expr, _Op, [L, _R]),
    color(Flags, Err, _, New),
    ml(New, L, M).

ml(Flags, right_elsewhere(Err, quote(Expr)), M) :-
    fix(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    color(Flags, Err, Color, New),
    ml(New, [L, color_or_box(Color, [Op, R])], M).

paren(Flags, right_elsewhere(_Err, quote(Expr)), P) :-
        paren(Flags, Expr, P).

math(_, expert(A = B), A -> B).
math(_, buggy(A \= B, _), A ~> B).

example :- example([highlight(err1)], instead_of(err1, sigma, sigma, s)).
example :- example([fix(err1)], instead_of(err1, sigma, sigma, s)).
example :- example([show(err1)], instead_of(err1, sigma, sigma, s)).

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
% denot(Flags, instead_of(Err, A, Instead, Of), W) :-
%     option(error(highlight), Flags, highlight),
%     !, denot(Flags, A, X),
%     denot(Flags, Instead, Y),
%     denot(Flags, Of, Z),
%     append([X, Y, Z], W).

denot(Flags, instead_of(Err, A, _, _), W) :-
    show(Flags, Err),
    !, denot(Flags, A, W).

denot(Flags, instead_of(Err, _, _, Of), W) :-
    fix(Flags, Err),
    !, denot(Flags, Of, W).

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

paren(Flags, '100%'(A), P) :-
    paren(Flags, A, P).

prec(Flags, '100%'(A), atomic-P) :-
    precedence(Flags, a*A, _-P).

%math(_, sup(sub(A, B), C), Subsup) :-
%    !, Subsup = subsup(A, B, C).
%
% Unclear if math works both ways
% math(Flags, sup(Sub, C), subsup(A, B, C)) :-
%     math(Flags, Sub, sub(A, B)).
%
%math(_, sub(sup(A, B), C), Subsup) :-
%    !, Subsup = subsup(A, C, B).
%
% Unclear if math works both ways
%math(Flags, sub(Sup, C), subsup(A, C, B)) :-
%    math(Flags, Sup, sup(A, B)).

ml(Flags, sup(Sub, C), M) :-
    precedence(Flags, Sub, sub-_),
    !, ml([replace(sub(A, B), subsup(A, B, C)) | Flags], Sub, M).

paren(Flags, sup(Sub, C), P) :-
    precedence(Flags, Sub, sub-_),
    !, paren([replace(sub(A, B), subsup(A, B, C)) | Flags], Sub, P).

prec(Flags, sup(Sub, C), P) :-
    precedence(Flags, Sub, sub-_),
    !, precedence([replace(sub(A, B), subsup(A, B, C)) | Flags], Sub, P).

ml(Flags, sub(Sup, C), M) :-
    precedence(Flags, Sup, sup-_),
    !, ml([replace(sup(A, B), subsup(A, C, B)) | Flags], Sup, M).

paren(Flags, sub(Sup, C), P) :-
    precedence(Flags, Sup, sup-_),
    !, paren([replace(sup(A, B), subsup(A, C, B)) | Flags], Sup, P).

prec(Flags, sub(Sup, C), P) :-
    precedence(Flags, Sup, sup-_),
    !, precedence([replace(sup(A, B), subsup(A, C, B)) | Flags], Sup, P).

ml(Flags, subsup(A, B, C), msubsup([X, Y, Z])) :-
    precedence(Flags, subsup(A, B, C), _-P),
    precedence(Flags, A, _-Inner),
    ( P =< Inner
      -> ml(Flags, paren(A), X)
      ; ml(Flags, A, X)
    ), ml(Flags, B, Y),
    ml(Flags, C, Z).

paren(Flags, subsup(A, _, _), P) :-
    paren(Flags, A, P).

prec(Flags, subsup(X, _, Z), P) :-
    precedence(Flags, X^Z, P).

math(_, A '_' B, sub(A, B)).

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

math(_, A^B, sup(A, B)).

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
math(Flags, A * B, M) :-
    paren(Flags, A / x, 0),
    !, M = A invisible_times B.

% Negative sign has same precedence as binary minus
math(Flags, -A, operator(P, fx, -, A)) :-
    precedence(Flags, a-b, _-P).

% Prefix and postfix operators (e.g., factorial)
math(_, Comp, operator(Prec, Fix, Op, A)) :-
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

math(_, Comp, operator(Prec, Fix, Op, A, B)) :-
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
    precedence(Flags, -A, P).

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
    precedence(Flags, T, P).

ml(Flags, round(A), M) :-
    ml([round(g) | Flags], A, M).

paren(Flags, round(A), P) :-
    paren(Flags, A, P).

ml(Flags, round0(A), M) :-
    ml([round(0) | Flags], A, M).

paren(Flags, round0(A), P) :-
    paren(Flags, A, P).

ml(Flags, round1(A), M) :-
    ml([round(1) | Flags], A, M).

paren(Flags, round1(A), P) :-
    paren(Flags, A, P).

ml(Flags, round2(A), M) :-
    ml([round(2) | Flags], A, M).

paren(Flags, round2(A), P) :-
    paren(Flags, A, P).

ml(Flags, round3(A), M) :-
    ml([round(3) | Flags], A, M).

paren(Flags, round3(A), P) :-
    paren(Flags, A, P).

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

prec(Flags, frac(A, B), frac-P) :-
    precedence(Flags, A/B, _-P).

% Large fraction
ml(Flags, dfrac(A, B), mstyle(displaystyle(true), X)) :-
    ml(Flags, frac(A, B), X).

paren(Flags, dfrac(A, B), P) :-
    paren(Flags, frac(A, B), P).

prec(Flags, dfrac(A, B), P) :-
    precedence(Flags, frac(A, B), P).

ml(Flags, choose(A, B),
        mrow([mo('('), mfrac(linethickness(0), [X, Y]), mo(')')])) :-
    ml(Flags, A, X),
    ml(Flags, B, Y).

paren(_, choose(_, _), 1).

prec(Flags, choose(A, B), paren-P) :-
    precedence(Flags, A^B, _-P).

ml(Flags, dchoose(A, B),
       mrow([mo('('), mfrac(mstyle([displaystyle(true), linethickness(0)]),
                            [X, Y]), mo(')')])) :-
    ml(Flags, A, X),
    ml(Flags, B, Y).

paren(Flags, dchoose(A, B), P) :-
    paren(Flags, choose(A, B), P).

prec(Flags, dchoose(A, B), P) :-
    precedence(Flags, choose(A, B), P).

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
    precedence(Flags, Arg, _-0),
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
    precedence(Flags, Arg, _-0),
    !, paren(Flags, Arg, P).

paren(Flags, trig(_, Arg), P) :-
    paren(Flags, Arg, Paren),
    P is Paren + 1.

prec(Flags, trig(Fun, Arg), trig-P) :-
    precedence(Flags, Fun*Arg, _-Prec),
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

math(_, tgroups_tratio(M_A, S_A, N_A, M_B, S_B, N_B, Mu),
     fun("independent t-test", (M_A, S_A, M_B, S_B; Mu, N_A, N_B))).

math(_, var_pool(V_A, N_A, V_B, N_B),
     fun(sub("Var", "pool"), (V_A, V_B; N_A, N_B))).

ml(Flags, sum(I, From, To, A), mrow([munderover([\['&sum;'], XFrom, XTo]), X])) :-
    ml(Flags, I = From, XFrom),
    ml(Flags, To, XTo),
    ml(Flags, A, X).

paren(Flags, sum(_, _, _, A), P) :-
    paren(Flags, A, P).

prec(Flags, sum(_, _, _, _), P) :-
    precedence(Flags, x + y, P).

ml(Flags, argmin(I, _, _, A), M) :-
    ml(Flags, fun(under("argmin", I), A), M).

paren(Flags, argmin(_, _, _, A), P) :-
    paren(Flags, A, P).

prec(Flags, argmin(_, _, _, _), P) :-
    precedence(Flags, x + y, P).

ml(Flags, argmax(I, _, _, A), M) :-
    ml(Flags, fun(under("argmax", I), A), M).

paren(Flags, argmax(_, _, _, A), P) :-
    paren(Flags, A, P).

prec(Flags, argmax(_, _, _, _), P) :-
    precedence(Flags, x + y, P).

ml(Flags, under(A, B), munder([X, Y])) :-
    ml(Flags, A, X),
    ml(Flags, B, Y).

paren(Flags, under(A, _), P) :-
    paren(Flags, A, P).

prec(Flags, under(A, _), accent-P) :-
    precedence(Flags, A, _-P).

ml(Flags, sqrt(A), msqrt(X)) :-
    ml(Flags, A, X).

paren(_, sqrt(_), 0).

prec(Flags, sqrt(_), P) :-
    precedence(Flags, x^y, P).

% t-distribution
math(_, tt(T, DF), fun('P', (abs('T') >= T ; "df" = DF))).
math(_, ut(T, DF), fun('P', ('T' >= T ; "df" = DF))).
math(_, pt(T, DF), fun('P', ('T' =< T ; "df" = DF))).

math(_, instead_of(Err, pt(PT, PT, DF), tt(TT, TT, DF)),
    fun('P', (instead_of(Err, 'T' =< PT, 'T' =< PT, abs('T') >= TT) ; "df" = DF))).

math(_, instead_of(Err, ut(UT, UT, DF), tt(TT, TT, DF)),
    fun('P', (instead_of(Err, 'T' >= UT, 'T' >= UT, abs('T') >= TT) ; "df" = DF))).

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

paren(Flags, fun(_, Args), P) :-
    paren(Flags, paren(Args), P).

prec(Flags, fun(_, _), fun-P) :-
    precedence(Flags, x^y, _-P).

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
