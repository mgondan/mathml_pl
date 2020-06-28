:- module(mathml, [
    mathml/2,
    mathml/3,
    mml//1,
    mml//2,
    palette/2,
    op(400, yfx, invisible_times),
    op(180, xf, !),
    op(170, xf, '%'),
    op(200, xfy, '_'),
    op(500, yfx, '+-'),
    op(1050, xfy, '~>'),
    op(600, xfy, '~'),
    op(150, xfx, ...)]).

:- op(180, xf, !).
:- op(170, xf, '%').
:- op(200, xfy, '_').
:- op(500, yfx, '+-').
:- op(400, yfx, invisible_times).
:- op(1050, xfy, '~>').
:- op(600, xfy, ~).

:- discontiguous ml/3.
:- discontiguous paren/3.
:- discontiguous prec/3.
:- discontiguous math/4.
:- discontiguous example/0.
:- discontiguous denot/3.

:- use_module(library(quantity)).
:- use_module(library(http/html_write)).

%
% Interface
%
mml(A) -->
    mml([color-auto, highlight(all)], A).
    
mml(Flags, A) -->
    { select(color-auto, Flags, Removed),
      !, 
      palette(A, P),
      append(Removed, P, New)
    }, 
    mml(New, A).

mml(Flags, A) -->
    { ml(Flags, A, M) },
    html(math(M)).

mathml(A, M) :-
    mathml([color-auto, highlight(all)], A, M).
    
mathml(Flags, A, M) :-
    select(color-auto, Flags, Removed),
    !, 
    palette(A, P),
    append(Removed, P, New),
    mathml(New, A, M).

mathml(Flags, A, Math) :-
    denoting(Flags, A, []),
    ml(Flags, A, M),
    !, Math = math(M).

mathml(Flags, A, Math) :-
    denoting(Flags, A, [H | T]),
    ml(Flags, A, M),
    !, Math = p([math(M), " ", H | T]).

mathml(Flags, A, Err) :-
    format(string(Err), "Conversion failed: ~k", mathml(Flags, A)).
    
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
:- multifile math_hook/4.

ml(Flags, A, M) :-
    ml_hook(Flags, A, M),
    !.

math(Flags, A, New, M) :-
    math_hook(Flags, A, New, M),
    !.

%
% Macros (e.g., red(X) for color("red", X)
%
ml(Flags, A, M) :-
    math(Flags, A, New, X),
    !, ml(New, X, M).

paren(Flags, A, P) :-
    math(Flags, A, New, X),
    !, paren(New, X, P).

%
% Precedence
%
% Macros
precedence(Flags, A, P) :-
    math(Flags, A, New, X),
    !, precedence(New, X, P).

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
math(Flags, A, Flags, punct(A)) :-
    punctuation(A, _).

punctuation(' ', mspace(width(thickmathspace), [])).
punctuation('_', &(nbsp)).
punctuation(ldots, mi(&(hellip))).
punctuation(cdots, mi(&(ctdot))).

ml(_, punct(A), M) :-
    punctuation(A, M).

paren(_, punct(_), 0).

prec(_, punct(_), punct-0).

example :- 
    example(' ').
    
example :- 
    example('_').

%
% Operators
%
math(Flags, A, Flags, op(A)) :-
    operator(A, _).

ml(_, op(A), mo(M)) :-
    operator(A, M).

paren(_, op(_), 0).

prec(_, op(_), op-0).

operator(+, +).
operator(-, -).
operator(*, &(sdot)).
operator(/, /).
operator(=\=, &('#8203')).
operator(=, =).
operator(<, &(lt)).
operator(>, &(gt)).
operator(=<, &(le)).
operator(>=, &(ge)).
operator(\=, &(ne)).
operator(!, !).
operator('', '').
operator('+-', &('PlusMinus')).
operator('%', '%').
operator(',', &(comma)).
operator(';', &('#59')).
operator('|', '|').
operator(invisible_times, &('#x2062')).
operator(->, &(rArr)).
operator(~>, &(zigrarr)).
operator(~, ~).
operator(:, :).
operator(..., &(hellip)).

example :- 
    example(/).

%
% Greek letters
%
% Translate to clean representation
math(Flags, A, Flags, greek(A)) :-
    greek(A).

greek(alpha).
greek(mu).
greek(pi).
greek(sigma).

% Translate to mathml
ml(_, greek(A), mi(&(A))).

% Level of parentheses
paren(_, greek(_), 0).

% Precedence
prec(_, greek(_), greek-0).

example :- 
    example(alpha).

%
% General atoms ("identifiers")
%
% Translate to clean representation
math(Flags, A, Flags, atom(A)) :-
    atom(A).

% Translate to mathml
ml(_, atom(A), mi(A)).

% Level of parentheses
paren(_, atom(_), 0).

% Precedence
prec(_, atom(_), atom-0).

example :-
    example(k).

%
% Strings (non-italicized)
%
% Translate to clean representation
math(Flags, A, Flags, string(A)) :-
    string(A).

% Translate to mathml
ml(_, string(A), mtext(A)).

% Level of parentheses
paren(_, string(_), 0).

% Precedence
prec(_, string(_), string-0).

example :- 
    example("text").

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
math(Flags, [], Flags, '').
math(Flags, [H | T], Flags, list(Sep, [H | T])) :-
    member(sep-Sep, Flags).

math(Flags, [H | T], Flags, list(',', [H | T])).
    
math(Flags, (H, T), Flags, list('', [H, T])).
math(Flags, (H; T), Flags, list(';', [H, T])).
math(Flags, (H| T), Flags, list('|', [H, T])).

ml(Flags, list(Sep, List), M) :-
    exclude({Flags}/[add(Err, _)] >> correct(Flags, Err), List, List1),
    exclude({Sep}/[[]] >> (Sep=(+)), List1, New),
    ml(Flags, list_(Sep, New), M).

ml(Flags, list_(Sep, [H | T]), mrow([HX | TX])) :-
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

prec(_, list(Sep, [_, _ | _]), P) :-
    current_op(Prec, _, Sep),
    !, P = list-Prec.

prec(_, list(_, _), list-0).

example :- 
    example([atom(x), atom(y), atom(z)]).

example :- example((x, y, z)).
example :- example((x ; y, z)).
example :- example((x | y, z)).
example :- example(paren([paren(x), paren(y)])).

%
% Colors
%
color(0, "red").
color(1, "blue").
color(2, "#00BF00").
color(3, "#9F5F00").
color(4, "#7F007F").
color(5, "#007F7F").

color(Flags, Code, Color) :-
    member(color(Code, Color), Flags).

palette(A, Flags) :-
    erroneous(A, Errs),
    sort(Errs, Errors),
    findall(color(E, C), (nth0(N, Errors, E), N6 is N mod 6, color(N6, C)), Flags).
    
math(Flags, red(A), Flags, color("red", A)).
math(Flags, green(A), Flags, color("green", A)).
math(Flags, blue(A), Flags, color("blue", A)).
math(Flags, black(A), Flags, color("black", A)).
math(Flags, grey(A), Flags, color("grey", A)).
math(Flags, lightred(A), Flags, color("#FFA0A0", A)).

ml(Flags, color(String, A), mstyle(mathcolor(String), X)) :-
    string(String),
    ml(Flags, A, X).

ml(Flags, color(Num, A), X) :-
    number(Num),
    color(Num, String),
    ml(Flags, color(String, A), X).
    
ml(Flags, color(Atom, A), X) :-
    atom(Atom),
    member(color(Atom, String), Flags),
    ml(Flags, color(String, A), X).
    
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
    precedence(Flags, atom(a)*A, _-P).

% Proper average
math(Flags, mean(A), Flags, overline(A)).

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
math(Flags, cancel(Color, A), Flags, color(Color, strike(black(A)))).

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
    erroneous(A, [_ | _]),
    !, ml(Flags, color(Col, roundedbox(black(A))), X).
    
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
math(Flags, tratio(A, _Df), Flags, A).
math(Flags, prob(A), Flags, A).
math(Flags, perc(A), Flags, A).
math(Flags, natural(A), Flags, A).
math(Flags, pm(A, B), Flags, '+-'(A, B)).
math(Flags, paired_ci(D, S, N, Alpha), Flags, '+-'(D, fun(sub('T', 1-Alpha/2), (N-1, '_', "df")) * dfrac(S, sqrt(N)))).

math(Flags, format_pvalue(A), Flags, A).

math(Flags, fratio(A), Flags, A).
math(Flags, confint(A, _Digits), Flags, A).

math(Flags, quote(A), Flags, A).

example :- 
    example(cancel(red, atom('X'))).
    
example :- 
    example(paren([paren(red(x)), green(paren(y))])).
    
example :- 
    example(underbrace(s, list('', (string("instead of"), punct(' '), sigma)))).

%
% Mistakes
%
erroneous(left_landed(Err, A), Errors) :-
    !, erroneous(A, T),
    Errors = [Err | T].

erroneous(right_landed(Err, A), Errors) :-
    !, erroneous(A, T),
    Errors = [Err | T].

erroneous(omit_left(Err, A), Errors) :-
    !, erroneous(A, T),
    Errors = [Err | T].

erroneous(omit_right(Err, A), Errors) :-
    !, erroneous(A, T),
    Errors = [Err | T].

erroneous(buggy(Fb, A), Errors) :-
    !, compound_name_arguments(Fb, Err, _),
    erroneous(A, T),
    Errors = [Err | T].

erroneous(instead_of(Err, Instead, Of), Errors) :-
    !, erroneous(Instead, I),
    erroneous(Of, O),
    append([Err | I], O, Errors).

erroneous(instead_of(Err, Instead, _Instead, Of, _Of), Errors) :-
    !, erroneous(Instead, I),
    erroneous(Of, O),
    append([Err | I], O, Errors).

erroneous(add(Err, A), Errors) :-
    !, erroneous(A, E),
    Errors = [Err | E].

erroneous(omit(Err, A), Errors) :-
    !, erroneous(A, E),
    Errors = [Err | E].

erroneous(color(Err, A), Errors) :-
    atom(Err),
    !, erroneous(A, E),
    Errors = [Err | E].
    
erroneous(A, Errors) :-
    compound(A),
    !, compound_name_arguments(A, _, Args),
    maplist(erroneous, Args, Errs),
    append(Errs, Errors).

erroneous(_, []).

fix(Flags, Err) :-
    member(fix(Err), Flags).

fix(Flags, _) :-
    member(fix(all), Flags).

highlight(Flags, Err) :-
    member(highlight(Err), Flags).

highlight(Flags, _) :-
    member(highlight(all), Flags).

show(Flags, Err) :-
    member(show(Err), Flags).

show(Flags, _) :-
    member(show(all), Flags).

correct(Flags, Err) :-
    member(correct(Err), Flags).

correct(Flags, _) :-
    member(correct(all), Flags).

math(Flags, expert(_Feedback, A), Flags, A).
math(Flags, buggy(_Feedback, A), Flags, A).

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

% No more errors within protect
ml(Flags, protect(X), M) :-
    !, ml(Flags, X, M).

paren(Flags, protect(X), P) :-
    paren(Flags, X, P).

prec(Flags, protect(X), P) :-
    precedence(Flags, X, P).

% A instead of B
ml(Flags, instead_of(Err, Instead, _), M) :-
    show(Flags, Err),
    !, ml(Flags, color_or_box(Err, Instead), M).

paren(Flags, instead_of(Err, Instead, _), P) :-
    show(Flags, Err),
    paren(Flags, Instead, P).

prec(Flags, instead_of(Err, Instead, _), P) :-
    show(Flags, Err),
    precedence(Flags, Instead, P).

ml(Flags, instead_of(Err, _, Of), M) :-
    fix(Flags, Err),
    !, ml(Flags, color(Err, Of), M).

paren(Flags, instead_of(Err, _, Of), P) :-
    fix(Flags, Err),
    paren(Flags, Of, P).

prec(Flags, instead_of(Err, _, Of), P) :-
    fix(Flags, Err),
    precedence(Flags, Of, P).

ml(Flags, instead_of(Err, _, Of), M) :-
    correct(Flags, Err),
    !, ml(Flags, Of, M).

paren(Flags, instead_of(Err, _, Of), P) :-
    correct(Flags, Err),
    paren(Flags, Of, P).

prec(Flags, instead_of(Err, _, Of), P) :-
    correct(Flags, Err),
    precedence(Flags, Of, P).

% ml(Flags, instead_of(Err, Instead, Of), M) :-
%     highlight(Flags, Err),
%     ml(Flags, underbrace(Instead, (string("instead of"), punct(' '), Of)), M).

ml(Flags, instead_of(Err, Instead, Of), M) :-
    highlight(Flags, Err),
    !, ml(Flags, Instead, MInstead),
    % Fix errors in Of
    ml([fix(all) | Flags], (string("instead of"), punct(' '), Of), MOf),
    M = munder([munder(accentunder(true), [MInstead, mo(stretchy(true), &('UnderBrace'))]), MOf]).

paren(Flags, instead_of(Err, Instead, _), P) :-
    highlight(Flags, Err),
    paren(Flags, Instead, P).

prec(Flags, instead_of(Err, Instead, _), P) :-
    highlight(Flags, Err),
    precedence(Flags, Instead, P).

% A instead of B
ml(Flags, instead_of(Err, Instead, _, _, _), M) :-
    show(Flags, Err),
    !, ml(Flags, color_or_box(Err, Instead), M).

paren(Flags, instead_of(Err, Instead, _, _, _), P) :-
    show(Flags, Err),
    paren(Flags, Instead, P).

prec(Flags, instead_of(Err, Instead, _, _, _), P) :-
    show(Flags, Err),
    precedence(Flags, Instead, P).

ml(Flags, instead_of(Err, _, _, Of, _), M) :-
    fix(Flags, Err),
    !, ml(Flags, color(Err, Of), M).

paren(Flags, instead_of(Err, _, _, Of, _), P) :-
    fix(Flags, Err),
    paren(Flags, Of, P).

prec(Flags, instead_of(Err, _, _, Of, _), P) :-
    fix(Flags, Err),
    precedence(Flags, Of, P).

ml(Flags, instead_of(Err, _, _, Of, _), M) :-
    correct(Flags, Err),
    !, ml(Flags, Of, M).

paren(Flags, instead_of(Err, _, _, Of, _), P) :-
    correct(Flags, Err),
    paren(Flags, Of, P).

prec(Flags, instead_of(Err, _, _, Of, _), P) :-
    correct(Flags, Err),
    precedence(Flags, Of, P).

ml(Flags, underbrace(A, Under),
    munder([munder(accentunder(true), [X, mo(stretchy(true), &('UnderBrace'))]), Y])) :-
    ml(Flags, A, X),
    ml(Flags, Under, Y).

ml(Flags, instead_of(Err, Instead, Instead, _Of, Of), M) :-
    highlight(Flags, Err),
    !, ml(Flags, Instead, MInstead),
    % Fix errors in Of
    ml([fix(all) | Flags], (string("instead of"), punct(' '), Of), MOf),
    M = munder([munder(accentunder(true), [MInstead, mo(stretchy(true), &('UnderBrace'))]), MOf]).

ml(Flags, instead_of(Err, I, Instead, _Of, Of), M) :-
    highlight(Flags, Err),
    !, ml(Flags, I, MI),
    % Fix errors in Of
    ml([fix(all) | Flags], (Instead, punct(' '), string("instead of"), punct(' '), Of), MOf),
    M = munder([munder(accentunder(true), [MI, mo(stretchy(true), &('UnderBrace'))]), MOf]).


paren(Flags, instead_of(Err, Instead, _, _, _), P) :-
    highlight(Flags, Err),
    paren(Flags, Instead, P).

prec(Flags, instead_of(Err, Instead, _, _, _), P) :-
    highlight(Flags, Err),
    precedence(Flags, Instead, P).

% Omit element in list
ml(Flags, omit(Err, Elem), M) :-
    highlight(Flags, Err),
    ml(Flags, underbrace(Elem, string("omitted")), M).

ml(Flags, omit(Err, Elem), M) :-
    show(Flags, Err),
    ml(Flags, cancel(Err, Elem), M).

ml(Flags, omit(Err, Elem), M) :-
    fix(Flags, Err),
    ml(Flags, color(Err, Elem), M).

ml(Flags, omit(Err, Elem), M) :-
    correct(Flags, Err),
    ml(Flags, Elem, M).

paren(Flags, omit(_Err, Elem), P) :-
    paren(Flags, Elem, P).

% Add element to list
ml(Flags, add(Err, Elem), M) :-
    highlight(Flags, Err),
    ml(Flags, underbrace(Elem, string("added")), M).

ml(Flags, add(Err, Elem), M) :-
    show(Flags, Err),
    ml(Flags, color(Err, Elem), M).

ml(Flags, add(Err, Elem), M) :-
    fix(Flags, Err),
    ml(Flags, cancel(Err, Elem), M).

ml(Flags, add(Err, Elem), M) :-
    correct(Flags, Err),
    ml(Flags, cancel(Err, Elem), M).

paren(Flags, add(_Err, Elem), P) :-
    paren(Flags, Elem, P).

prec(Flags, add(_Err, Elem), P) :-
    precedence(Flags, Elem, P).

% Left part omitted
ml(Flags, omit_left(Err, Expr), M) :-
    highlight(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    ml(Flags, (underbrace((L, ' ', Op), string("omitted")), ' ', R), M).

ml(Flags, omit_left(Err, Expr), M) :-
    show(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    ml(Flags, (cancel(Err, (L, ' ', Op)), ' ', R), M).

ml(Flags, omit_left(Err, Expr), M) :-
    fix(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    ml(Flags, (color(Err, (L, ' ', Op)), ' ', R), M).

ml(Flags, omit_left(Err, Expr), M) :-
    correct(Flags, Err),
    ml(Flags, Expr, M).

paren(Flags, omit_left(_Err, Expr), P) :-
    paren(Flags, Expr, P).

ml(Flags, omit_right(Err, Expr), M) :-
    highlight(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    ml(Flags, (L, underbrace((Op, ' ', R), string("omitted"))), M).

ml(Flags, omit_right(Err, Expr), M) :-
    show(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    ml(Flags, (L, ' ', cancel(Err, (Op, ' ', R))), M).

ml(Flags, omit_right(Err, Expr), M) :-
    fix(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    ml(Flags, (L, ' ', color(Err, (Op, ' ', R))), M).

ml(Flags, omit_right(Err, Expr), M) :-
    correct(Flags, Err),
    ml(Flags, Expr, M).

paren(Flags, omit_right(_Err, Expr), P) :-
    paren(Flags, Expr, P).

ml(Flags, left_landed(Err, Expr), M) :-
    highlight(Flags, Err),
    precedence(Flags, Expr, _-Prec),
    compound_name_arguments(Expr, Op, [L, R]),
    ml(Flags, (color(Err, roundedbox(black((L, Op)))), operator(Prec, fy, '', R)), M).

%ml(Flags, left_landed(Err, Expr), M) :-
%    show(Flags, Err),
%    precedence(Flags, Expr, _-Prec),
%    compound_name_arguments(Expr, Op, [L, R]),
%    ml(Flags, (color_or_box(Err, (L, Op)), operator(Prec, fy, '', R)), M).

ml(Flags, left_landed(Err, Expr), M) :-
    show(Flags, Err),
    precedence(Flags, Expr, _-Prec),
    compound_name_arguments(Expr, Op, [L, R]),
    erroneous((L, Op), [_ | _]), % expression has error -> box
    !, ml(Flags, (color_or_box(Err, (L, Op)), operator(Prec, fy, '', R)), M).

ml(Flags, left_landed(Err, Expr), M) :-
    show(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    compound_name_arguments(New, Op, [L, black(R)]),
    ml(Flags, color(Err, New), M).

ml(Flags, left_landed(Err, Expr), M) :-
    fix(Flags, Err),
    compound_name_arguments(Expr, _, [_, R]),
    ml(Flags, R, M).

ml(Flags, left_landed(Err, Expr), M) :-
    correct(Flags, Err),
    compound_name_arguments(Expr, _, [_, R]),
    ml(Flags, R, M).

paren(Flags, left_landed(_Err, Expr), P) :-
    paren(Flags, Expr, P).

prec(Flags, left_landed(_Err, Expr), Prec) :-
    precedence(Flags, Expr, Prec).

ml(Flags, right_landed(Err, Expr), M) :-
    highlight(Flags, Err),
    precedence(Flags, Expr, _-Prec),
    compound_name_arguments(Expr, Op, [L, R]),
    ml(Flags, (operator(Prec, yf, '', L), color(Err, roundedbox(black((Op, R))))), M).

ml(Flags, right_landed(Err, Expr), M) :-
    show(Flags, Err),
    precedence(Flags, Expr, _-Prec),
    compound_name_arguments(Expr, Op, [L, R]),
    ml(Flags, (operator(Prec, yf, '', L), color_or_box(Err, operator(Prec, yfx, Op, '', R))), M).

ml(Flags, right_landed(Err, Expr), M) :-
    fix(Flags, Err),
    compound_name_arguments(Expr, _Op, [L, _R]),
    ml(Flags, L, M).

ml(Flags, right_landed(Err, Expr), M) :-
    correct(Flags, Err),
    compound_name_arguments(Expr, _Op, [L, _R]),
    ml(Flags, L, M).

paren(Flags, right_landed(_Err, Expr), P) :-
    paren(Flags, Expr, P).

prec(Flags, right_landed(_Err, Expr), Prec) :-
    precedence(Flags, Expr, Prec).

ml(Flags, left_elsewhere(Err, Expr), M) :-
    highlight(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    ml(Flags, (color(Err, roundedbox(strike(black((L, Op))))), R), M).

ml(Flags, left_elsewhere(Err, Expr), M) :-
    show(Flags, Err),
    compound_name_arguments(Expr, _Op, [_L, R]),
    ml(Flags, R, M).

ml(Flags, left_elsewhere(Err, Expr), M) :-
    fix(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    ml(Flags, (color_or_box(Err, (L, Op)), R), M).

ml(Flags, left_elsewhere(Err, Expr), M) :-
    correct(Flags, Err),
    ml(Flags, Expr, M).

paren(Flags, left_elsewhere(Err, Expr), P) :-
    show(Flags, Err),
    !, compound_name_arguments(Expr, _Op, [_L, R]),
    paren(Flags, R, P).

paren(Flags, left_elsewhere(_Err, Expr), P) :-
    paren(Flags, Expr, P).

prec(Flags, left_elsewhere(Err, Expr), Prec) :-
    show(Flags, Err),
    compound_name_arguments(Expr, _Op, [_L, R]),
    precedence(Flags, R, Prec).

ml(Flags, right_elsewhere(Err, Expr), M) :-
    highlight(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    ml(Flags, (L, color(Err, roundedbox(strike(black((Op, R)))))), M).

ml(Flags, right_elsewhere(Err, Expr), M) :-
    show(Flags, Err),
    compound_name_arguments(Expr, _Op, [L, _R]),
    ml(Flags, L, M).

ml(Flags, right_elsewhere(Err, Expr), M) :-
    fix(Flags, Err),
    compound_name_arguments(Expr, Op, [L, R]),
    ml(Flags, (L, color_or_box(Err, (Op, R))), M).

ml(Flags, right_elsewhere(Err, Expr), M) :-
    correct(Flags, Err),
    ml(Flags, Expr, M).

paren(Flags, right_elsewhere(Err, Expr), P) :-
    show(Flags, Err),
    !, compound_name_arguments(Expr, _Op, [L, _R]),
    paren(Flags, L, P).

paren(Flags, right_elsewhere(_Err, Expr), P) :-
        paren(Flags, Expr, P).

prec(Flags, right_elsewhere(Err, Expr), Prec) :-
    show(Flags, Err),
    compound_name_arguments(Expr, _Op, [L, _R]),
    precedence(Flags, L, Prec).

% Rounding errors
math(Flags, dec2(X), Flags, X).

example :- example([highlight(err1)], instead_of(err1, sigma, sigma, s)).
example :- example([fix(err1)], instead_of(err1, sigma, sigma, s)).
example :- example([show(err1)], instead_of(err1, sigma, sigma, s)).
example :- example([correct(err1)], instead_of(err1, sigma, sigma, s)).

% t-distribution
math(Flags, tt(T, DF), Flags, fun(atom('P'), (abs(atom('T')) >= T ; [DF, punct('_'), string("df")]))).
math(Flags, ut(T, DF), Flags, fun(atom('P'), (atom('T') >= T ; [DF, punct('_'), string("df")]))).
math(Flags, 2 * pt(T, DF, 'lower.tail'='FALSE'), Flags, tt(T, DF)).
math(Flags, pt(T, DF), Flags, fun(atom('P'), (atom('T') =< T ; [DF, punct('_'), string("df")]))).
math(Flags, pt(T, DF, 'lower.tail'='FALSE'), Flags, ut(T, DF)).
math(Flags, qt(P, DF), Flags, fun(subsup('T', "df"=DF, -1), P)).

%
% Units
%
is_unit(Flags, A) :-
    unit(Flags, A, _, _).

unit(_, A, Sep, M) :-
    unit(A, Sep, M).

unit('%', '', "%").
unit('kg', punct('_'), "kg").

ml(Flags, A, mrow([X, Sp, Unit])) :-
    compound(A),
    compound_name_arguments(A, U, [Num]),
    is_unit(Flags, U),
    unit(Flags, U, Sep, Unit),
    ml(Flags, Num, X),
    ml(Flags, Sep, Sp).

math(Flags, P '%', Flags, (P, "%")).

example :- example(10 '%').
example :- example('100%'(0.1)).
example :- example(kg(5)).

%
% Operators
%
ml(Flags, '100%'(A), X) :-
    select_option(mult(M), Flags, New, 1),
    _100M is 100 * M,
    ml([mult(_100M) | New], round(A) '%', X).

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

math(Flags, A '_' B, Flags, sub(A, B)).

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

% experimental: sin^2 x for simple x
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

paren(Flags, sup(A, _), Paren) :-
    current_op(Prec, xfy, ^),
    precedence(Flags, A, _-P),
    Prec > P,
    paren(Flags, A, Paren).

paren(Flags, sup(A, _), Paren) :-
    current_op(Prec, xfy, ^),
    precedence(Flags, A, _-P),
    Prec =< P,
    paren(Flags, A, Inner),
    Paren is Inner + 1.

prec(_, sup(_, _), sup-P) :-
    current_op(Prec, xfy, ^),
    P = Prec.

% Omit multiplication sign in simple products
math(Flags, A * B, Flags, M) :-
    current_op(Mult, yfx, *),
    precedence(Flags, A, OpA-PrecA),
    dif(OpA, frac),
    dif(OpA, dfrac),
    dif(OpA, /),
    dif(OpA, fun),
    PrecA =< Mult,
    paren(Flags, A, 0),
    precedence(Flags, B, OpB-PrecB),
    dif(OpB, frac),
    dif(OpB, dfrac),
    dif(OpB, /),
    dif(OpB, fun),
    PrecB =< Mult,
    paren(Flags, B, 0),
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
    ), ml(Flags, op(Op), F).

ml(Flags, operator(P, fy, Op, A), mrow([F, Y])) :-
    precedence(Flags, A, _-Inner),
    ( P < Inner
      -> ml(Flags, paren(A), Y)
      ; ml(Flags, A, Y)
    ), ml(Flags, op(Op), F).

ml(Flags, operator(P, xf, Op, A), mrow([X, F])) :-
    precedence(Flags, A, _-Inner),
    ( P =< Inner
      -> ml(Flags, paren(A), X)
      ; ml(Flags, A, X)
    ), ml(Flags, op(Op), F).

ml(Flags, operator(P, yf, Op, A), mrow([Y, F])) :-
    precedence(Flags, A, _-Inner),
    ( P < Inner
      -> ml(Flags, paren(A), Y)
      ; ml(Flags, A, Y)
    ), ml(Flags, op(Op), F).

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

prec(_, operator(P, _, Op, _), Op-P).

% Avoid unnecessary parentheses right to + in 1 + (2 - 3)
math(Flags, A + B, Flags, operator(P, yfy, +, A, B)) :-
    current_op(P, yfx, +).

math(Flags, A * B, Flags, operator(P, yfy, *, A, B)) :-
    current_op(P, yfx, *).

math(Flags, A invisible_times B, Flags, operator(P, yfy, invisible_times, A, B)) :-
    current_op(P, yfx, *).

math(Flags, A : B, Flags, operator(P, yfy, :, A, B)) :-
    current_op(P, xfy, :).

math(Flags, A = B, Flags, operator(P, yfy, =, A, B)) :-
    current_op(P, xfx, =).

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
    ), ml(Flags, op(Op), F).

ml(Flags, operator(P, xfy, Op, A, B), mrow([X, F, Y])) :-
    precedence(Flags, A, _-PrecA),
    ( P =< PrecA
      -> ml(Flags, paren(A), X)
      ; ml(Flags, A, X)
    ), precedence(Flags, B, _-PrecB),
    ( P < PrecB
      -> ml(Flags, paren(B), Y)
      ; ml(Flags, B, Y)
    ), ml(Flags, op(Op), F).

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
    ), ml(Flags, op(Op), F).

ml(Flags, operator(P, yfx, Op, A, B), mrow([Y, F, X])) :-
    precedence(Flags, A, _-PrecA),
    ( P < PrecA
      -> ml(Flags, paren(A), Y)
      ; ml(Flags, A, Y)
    ), precedence(Flags, B, _-PrecB),
    ( P =< PrecB
      -> ml(Flags, paren(B), X)
      ; ml(Flags, B, X)
    ), ml(Flags, op(Op), F).

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

paren(Flags, operator(Prec, yfy, _, A, B), P) :-
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

prec(_, operator(P, _, Op, _, _), Op-P).

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
ml(_, quantity(S), M) :-
    string(S),
    quantity(Q, Options, S),
    !, quantity_mathml(Q, Options, M).

paren(_Flags, quantity(S), P) :-
    string(S),
    quantity(Q, Options, S),
    !, quantity_paren(Q, Options, P).

prec(_Flags, quantity(S), P) :-
    string(S),
    quantity(Q, Options, S),
    !, quantity_prec(Q, Options, P).

ml(_, quantity(Q, Options), M) :-
    quantity_mathml(Q, Options, Mm),
    !, M = Mm.

paren(_Flags, quantity(Q, Options), P) :-
    quantity_paren(Q, Options, P).

prec(_Flags, quantity(Q, Options), P) :-
    quantity_prec(Q, Options, P).

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
% Fractions
%
ml(Flags, frac(A, B), mfrac([X, Y])) :-
    ml(Flags, A, X),
    ml(Flags, B, Y).

paren(_, frac(_, _), 0).

prec(Flags, frac(A, B), frac-Prec) :-
    precedence(Flags, A/B, _-P),
    Prec is P - 1.

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
math(Flags, sin(A), Flags, trig(sin, A)).
math(Flags, cos(A), Flags, trig(cos, A)).
math(Flags, tan(A), Flags, trig(tan, A)).

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
math(Flags, baseline_fratio(_, _Primary, _Covariates, _Strata, _Other, _Interactions, _Exclude, Therapy), Flags, sub(atom('F'), Therapy)).

math(Flags, ancova_f(_, _Primary, _Covariates, _Strata, _Other, _Interactions, _Exclude, Therapy), Flags, sub(atom('F'), Therapy)).
math(Flags, ancova_ff(_, _Primary, _Covariates, _Strata, _Other, _Interactions, _Exclude, Therapy), Flags, sub(atom('F'), Therapy)).
math(Flags, ancova_fff(_, _Primary, _Covariates, _Strata, _Other, _Interactions, _Exclude, Therapy), Flags, sub(atom('F'), Therapy)).
math(Flags, ancova_ffff(_, _Primary, _Covariates, _Strata, _Other, _Interactions, _Exclude, Therapy), Flags, sub(atom('F'), Therapy)).
math(Flags, ancova_fffff(_, _Primary, _Covariates, _Strata, _Other, _Interactions, _Exclude, Therapy), Flags, sub(atom('F'), Therapy)).
math(Flags, ancova_ffffff(_, _Primary, _Covariates, _Strata, _Other, _Interactions, _Exclude, Therapy), Flags, sub(atom('F'), Therapy)).
denot(Flags, ancova_ffffff(_, Primary, Covariates, Strata, Other, Interactions, Exclude, Therapy), W) :-
    !, denot(Flags, Primary, T1),
    denot(Flags, Covariates, T2),
    denot(Flags, Strata, T3),
    denot(Flags, Other, T4),
    denot(Flags, Interactions, T5),
    denot(Flags, Exclude, T6),
    denot(Flags, Therapy, T7),
    append([T1, T2, T3, T4, T5, T6, T7], T),
    W = [denoting(tilde(Primary, [Covariates, Strata, Other, Interactions, Exclude, Therapy]), "the statistical model") | T].

ml(Flags, Tilde, M) :-
    compound(Tilde),
    compound_name_arguments(Tilde, tilde, [Dependent | Predictors]),
    subtract(Predictors, [[]], NonEmpty),
    !, ml([sep-(+) | Flags], Dependent ~ NonEmpty, M).

paren(Flags, Tilde, P) :-
    compound(Tilde),
    compound_name_arguments(Tilde, tilde, [Dependent | Predictors]),
    subtract(Predictors, [[]], NonEmpty),
    !, paren(Flags, Dependent ~ NonEmpty, P).
    
math(Flags, lm(Model, _Data), Flags, Model).

math(Flags, anova_f(_, Therapy), Flags, sub(atom('F'), Therapy)).

math(Flags, paired_t(D, Mu, S, N),
     Flags, fun((string("paired"), punct('_'), atom(t), string("-test")), [D, S; N, Mu])).

math(Flags, groups_t(M_A, S_A, N_A, M_B, S_B, N_B),
     Flags, fun((string("independent"), punct('_'), atom(t), string("-test")), [M_A, S_A, M_B, S_B; N_A, N_B])).

math(Flags, var_pool(V_A, N, V_B, N), Flags, dfrac(V_A + V_B, 2)).

math(Flags, var_pool(V_A, N_A, V_B, N_B), Flags, dfrac((N_A-1)*V_A + (N_B-1)*V_B, N_A + N_B - 2)).

ml(Flags, sum(I, From, To, A), mrow([munderover([\['&sum;'], XFrom, XTo]), X])) :-
    ml(Flags, I = From, XFrom),
    ml(Flags, To, XTo),
    ml(Flags, A, X).

paren(Flags, sum(_, _, _, A), P) :-
    paren(Flags, A, P).

prec(Flags, sum(_, _, _, _), P) :-
    precedence(Flags, x + y, P).

ml(Flags, argmin(I, A), M) :-
    ml(Flags, fun(under(string("argmin"), I), A), M).

paren(Flags, argmin(_, A), P) :-
    paren(Flags, A, P).

prec(Flags, argmin(_, A), P) :-
    precedence(Flags, x + A, P).

ml(Flags, argmax(I, A), M) :-
    ml(Flags, fun(under(string("argmax"), I), A), M).

paren(Flags, argmax(_, A), P) :-
    paren(Flags, A, P).

prec(Flags, argmax(_, A), P) :-
    precedence(Flags, x + A, P).

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

math(Flags, dbinom(K, N, P), Flags, fun(sub(atom('P'), string("Bi")), [atom('X') = K ; [N, P]])).
math(Flags, pbinom(K, N, P), Flags, fun(sub(atom('P'), string("Bi")), [atom('X') =< K ; [N, P]])).
math(Flags, ubinom(K, N, P), Flags, fun(sub(atom('P'), string("Bi")), [atom('X') >= K ; [N, P]])).

math(Flags, instead_of(Code, pbinom(K, N, P), ubinom(K, N, P)),
     Flags, fun(sub(atom('P'), string("Bi")), [instead_of(Code, atom('X') =< K, atom('X') >= K) ; [N, P]])).

math(Flags, instead_of(Code, dbinom(K, N, P), ubinom(K, N, P)),
     Flags, fun(sub(atom('P'), string("Bi")), [instead_of(Code, atom('X') = K, atom('X') >= K) ; [N, P]])).

% Lower and upper critical value
math(Flags, lower(Alpha), Flags, Alpha).
math(Flags, upper(Alpha), Flags, 1 - Alpha).

math(Flags, uqbinom(Tail, Dist, Alpha, N, P), Flags, fun(Tail, fun(sub('P', "Bi"), [Dist ; [N, P]]) =< Alpha)).
math(Flags, pwbinom(K, Dist, N, P), Flags, fun(sub('P', "Bi"), [dist(Dist, K) ; [N, P]])).

math(Flags, tail("upper"), Flags, under("argmin", k)).
math(Flags, tail("lower"), Flags, under("argmax", k)).

math(Flags, dist("upper"), Flags, 'X' >= k).
math(Flags, dist("lower"), Flags, 'X' =< k).
math(Flags, dist("density"), Flags, 'X' = k).

math(Flags, dist("upper", K), Flags, 'X' >= K).
math(Flags, dist("lower", K), Flags, 'X' =< K).
math(Flags, dist("density", K), Flags, 'X' = K).

math(Flags, dist1(Dist), Flags, dist(Dist)).

math(Flags, bernoulli(Succ, N, Pi), Flags, successes(Succ, Pi) * failures(N-Succ, Pi)).
math(Flags, successes(Succ, Pi), Flags, Pi^Succ).
math(Flags, failures(Fail, Pi), Flags, (1-Pi)^Fail).

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

%
% Abbreviations
%
math(Flags, denoting(A, X, _), Flags, color_or_box(Err, A)) :-
    erroneous(X, [Err | _]).

math(Flags, denoting(A, _, _), Flags, A).

math(Flags, denoting(_, _), Flags, '').

math(Flags, abbrev(A, _), Flags, A).

% Collect abbreviations
abbreviations(Flags, A, W) :-
    denot(Flags, A, X),
    list_to_set(X, W).

denot(_, A, []) :-
    atomic(A).

denot(Flags, denoting(Abbrev, Expr, Desc), W) :-
    !, denot(Flags, Expr, T),
    W = [denoting(Abbrev = Expr, Desc) | T].

denot(Flags, denoting(Expr, Desc), W) :-
    !, denot(Flags, Expr, T),
    W = [denoting(Expr, Desc) | T].

% See compound below
%
% denot(Flags, instead_of(Err, A, Instead, Of), W) :-
%     option(error(highlight), Flags, highlight),
%     !, denot(Flags, A, X),
%     denot(Flags, Instead, Y),
%     denot(Flags, Of, Z),
%     append([X, Y, Z], W).

denot(Flags, instead_of(Err, Instead, _), W) :-
    show(Flags, Err),
    !, denot(Flags, Instead, W).

denot(Flags, instead_of(Err, _, Of), W) :-
    fix(Flags, Err),
    !, denot(Flags, Of, W).

denot(Flags, instead_of(Err, _, Of), W) :-
    correct(Flags, Err),
    !, denot(Flags, Of, W).

denot(Flags, instead_of(Err, Instead, _, _, _), W) :-
    show(Flags, Err),
    !, denot(Flags, Instead, W).

denot(Flags, instead_of(Err, _, _, Of, _), W) :-
    fix(Flags, Err),
    !, denot(Flags, Of, W).

denot(Flags, instead_of(Err, _, _, Of, _), W) :-
    correct(Flags, Err),
    !, denot(Flags, Of, W).

denot(Flags, [H | T], With) :-
    !, maplist(denot(Flags), [H | T], List),
    append(List, With).

%denot(Flags, A, W) :-
%    math(Flags, A, New, B),
%    !, denot(New, B, W).

denot(Flags, Comp, With) :-
    compound(Comp),
    compound_name_arguments(Comp, _, Args),
    maplist(denot(Flags), Args, List),
    append(List, With).

% Render abbreviations
denoting(Flags, A, Empty) :-
    abbreviations(Flags, A, []),
    !, Empty = [].

denoting(Flags, A, [M]) :-
    abbreviations(Flags, A, [denoting(Expr, Des)]),
    !, ml(Flags, Expr, MExpr),
    M = span(["with", &(nbsp), math(MExpr), " denoting ", Des, "."]).
    
denoting(Flags, A, [M | MT]) :-
    abbreviations(Flags, A, [denoting(Expr, Des) | T]),
    !, ml(Flags, Expr, MExpr),
    M = span(["with", &(nbsp), math(MExpr), " denoting ", Des, ", "]),
    and(Flags, T, MT).

and(_, [], ["."]).

and(Flags, [denoting(Expr, Des) | T], [M | MT]) :-
    ml(Flags, Expr, MExpr),
    M = span(["and", &(nbsp), math(MExpr), " denoting ", Des, ", "]),
    and(Flags, T, MT).
