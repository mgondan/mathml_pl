:- module(mathml, [
    mathml/2,
    mathml//2,
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

:- discontiguous mathml//2.
:- discontiguous example/0.
:- discontiguous paren//2.
:- discontiguous prec//2.
:- discontiguous math//2.

:- use_module(library(quantity)).

%
% Hook for custom atoms, e.g., s_D -> sub(s, 'D')
%
% atom_hook(s_D, X) -->
%    mathml(sub(s, 'D'), X).
%
:- multifile atom_hook//2.

%
% Interface
%
mml(A) -->
    mml([error-highlight], A).
    
mml(Flags, A) -->
    {phrase(mathml(A, M), [Flags], _)},
    html(math(M)).

mathml(A, M) :-
    mathml([error-highlight], A, M).

mathml(Flags, A, math(X)) :-
    phrase(denoting(A, []), [Flags], _),
    phrase(mathml(A, X), [Flags], _).

mathml(Flags, A, math([mrow([M, ',']),
        mspace(width(thickmathspace), ''), H | T])) :-
    phrase(denoting(A, [H | T]), [Flags], _),
    phrase(mathml(A, M), [Flags], _).

%
% Show example
%
example(A) :-
    example([error-highlight], A).

example(Flags, A) :-
    mathml(Flags, A, M) -> writeln(math:A = ml:M) ;  writeln(math:A = failed).

%
% For SWISH
%
% example(Flags, A) :-
%     mathml(Flags, A, M) -> html(html(math(M))) ; writeln(failed).

%
% Helper predicate for flags
%
state(S), [S] --> [S].
state(S0, S), [S] --> [S0].

%
% Macros (e.g., red(X) for color(red, X)
%
mathml(A, M) -->
    math(A, X),
    !, mathml(X, M).

paren(A, Paren) -->
    math(A, X),
    !, paren(X, Paren).

prec(A, Prec) -->
    math(A, X),
    !, prec(X, Prec).

%
% Punctuation
%
is_punct(A) --> punct(A, _).
punct(A, M) --> {punct(A, M)}.

punct('_', &(nbsp)).
punct(' ', mspace(width(thickmathspace), '')).
punct(ldots, mi(&(hellip))).
punct(cdots, mi(&(ctdot))).

mathml(A, M) -->
    is_punct(A),
    punct(A, M).

paren(A, 0) -->
    is_punct(A).

prec(A, 0) -->
    is_punct(A).

example :- example(' ').
example :- example('_').

%
% Operators
%
is_op(A) --> op(A, _).
op(A, mo(M)) --> {op(A, M)}.

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

mathml(A, M) -->
    is_op(A),
    op(A, M).

paren(A, 0) --> is_op(A).
prec(A, 0) --> is_op(A).

example :- example(/).

%
% Identifiers
%
is_id(A) --> id(A, _).
id(A, mi(M)) --> {id(A, M)}.

id(alpha, &(alpha)).
id(mu, &(mu)).
id(pi, &(pi)).
id(sigma, &(sigma)).

mathml(A, M) -->
    is_id(A),
    id(A, M).

paren(A, 0) --> is_id(A).
prec(A, 0) --> is_id(A).

example :- example(alpha).

%
% General atoms
%
is_atom(A) -->
    atom(A, _),
    \+ is_id(A),
    \+ is_punct(A),
    \+ is_op(A).

atom(A, X) --> atom_hook(A, X), !. 
atom(A, mi(A)) --> {atom(A)}.

mathml(A, M) -->
    is_atom(A),
    atom(A, M).

paren(A, 0) --> is_atom(A).
prec(A, 0) --> is_atom(A).

example :- example(k).

%
% Strings (non-italicized)
%
is_string(A) --> string(A, _).
string(A, mtext(A)) --> {string(A)}.

mathml(A, M) -->
    is_string(A),
    string(A, M).

paren(A, 0) --> is_string(A).
prec(A, 0) --> is_string(A).

example :- example("text").

%
% Parentheses
%
mathml(paren(A), M) -->
    paren(A, 0),
    mathml(parentheses(A), M).

mathml(paren(A), M) -->
    paren(A, 1),
    mathml(bracket(A), M).

mathml(paren(A), M) -->
    paren(A, 2),
    mathml(curly(A), M).

mathml(paren(A), M) -->
    paren(A, P),
    {P > 2},
    mathml(parentheses(A), M).

paren(paren(A), P) -->
    paren(A, P0),
    {P is P0 + 1}.

paren(parentheses(_), 1) --> [].
paren(bracket(_), 2) --> [].
paren(curly(_), 3) --> [].

prec(paren(_), 0) --> [].
prec(parentheses(_), 0) --> [].
prec(bracket(_), 0) --> [].
prec(curly(_), 0) --> [].

mathml(parentheses(A), mfenced(X)) -->
    mathml(A, X).

mathml(bracket(A), mfenced([open('['), close(']')], X)) -->
    mathml(A, X).

mathml(curly(A), mfenced([open('{'), close('}')], X)) -->
    mathml(A, X).

mathml(abs(A), mfenced([open('|'), close('|')], X)) -->
    mathml(A, X).

paren(abs(A), Paren) --> 
    paren(A, Paren).

prec(abs(A), Prec) --> 
    prec(parentheses(A), Prec).

example :- example(paren(x)).
example :- example(paren(bracket(x))).
example :- example(paren(paren(x))).

%
% Lists (e.g., function arguments)
%
math([H | T], list('', [H | T])) --> [].

math((H, T), list(',', [H, T])) --> [].

math((H; T), list(';', [H, T])) --> [].

math((H | T), list('|', [H, T])) --> [].

mathml(list(Sep, List), mfenced([open(''), close(''), separators(Sep)], L)) -->
    state(S, New),
    {maplist({S, New}/[A, X] >> mathml(A, X, [S], [New]), List, L)}.

paren(list(_, List), Paren) -->
    state(S, New),
    {maplist({S, New}/[A, X] >> paren(A, X, [S], [New]), List, P),
     sort(0, @>, P, [Paren | _])}.

prec(list(Sep, _), Prec) -->
    {current_op(P, _, Sep), Prec = P -> true ; Prec = 1}.

example :- example([x, y, z]).
example :- example((x, y, z)).
example :- example((x ; y, z)).
example :- example((x | y, z)).
example :- example(paren([paren(x), paren(y)])).

%
% Decorations
%
math(red(A), color(red, A)) --> [].
math(green(A), color(green, A)) --> [].
math(blue(A), color(blue, A)) --> [].
math(black(A), color(black, A)) --> [].

mathml(color(Col, A), mstyle(color(Col), X)) -->
        mathml(A, X).

paren(color(_, A), Paren) -->
        paren(A, Paren).

prec(color(_, A), Prec) -->
        prec(A, Prec).

% Only decoration
mathml(overline(A), mover(accent(true), [X, mo(&(macr))])) -->
        mathml(A, X).

paren(overline(A), Paren) -->
        paren(A, Paren).

prec(overline(A), Prec) -->
        prec(A, Prec).

% Proper average
math(mean(A), overline(A)) --> [].

mathml(underbrace(A, Under), munder([munder(accentunder(true), [X, mo(stretchy(true), &('UnderBrace'))]), Y])) -->
    mathml(A, X),
    mathml(Under, Y).

paren(underbrace(A, _), Paren) -->
        paren(A, Paren).

prec(underbrace(A, _), Prec) -->
        prec(A, Prec).

math(cancel(A), red(strike(black(A)))) --> [].

mathml(strike(A), menclose(notation(updiagonalstrike), X)) -->
    mathml(A, X).

paren(strike(A), Paren) -->
        paren(A, Paren).

prec(strike(A), Prec) -->
        prec(A, Prec).

math(format_tratio(A), round2(A)) --> 
    {number(A)}.

math(format_tratio(A), A) --> 
    {\+ number(A)}.

math(format_pvalue(A), round2(A)) --> 
    {number(A)}.

math(format_pvalue(A), A) --> 
    {\+ number(A)}.

math(quote(A), A) --> [].

example :- example(cancel('X')).
example :- example(paren([paren(red(x)), green(paren(y))])).
example :- example(underbrace(s, list('', ["instead of", ' ', sigma]))).

%
% Mistakes
%
mathml(error(Err, A), M) -->
    state(S),
    {subtract(S, [error-_], New)},
    state(S, [error-Err | New]),
    mathml(A, M),
    state([error-Err | New], S).
  
paren(error(Err, A), Paren) -->
    state(S),
    {subtract(S, [error-_], New)},
    state(S, [error-Err | New]),
    paren(A, Paren),
    state([error-Err | New], S).
  
prec(error(Err, A), Prec) -->
    state(S),
    {subtract(S, [error-_], New)},
    state(S, [error-Err | New]),
    prec(A, Prec),
    state([error-Err | New], S).
  
mathml(instead_of(A, B), M) -->
        state(S),
        {member(error-highlight, S)},
        mathml(underbrace(A, ["instead of", ' ', B]), M).

mathml(instead_of(A, _), M) -->
        state(S),
        {member(error-show, S)},
        mathml(red(A), M).

mathml(instead_of(_, B), M) -->
        state(S),
        {member(error-fix, S)},
        mathml(green(B), M).

paren(instead_of(A, _), Paren) -->
        state(S),
        {member(error-highlight, S)},
        paren(A, Paren).

paren(instead_of(A, _), Paren) -->
        state(S),
        {member(error-show, S)},
        paren(A, Paren).

paren(instead_of(_, B), Paren) -->
        state(S),
        {member(error-fix, S)},
        paren(B, Paren).

prec(instead_of(A, _), Prec) -->
        state(S),
        {member(error-highlight, S)},
        prec(A, Prec).

prec(instead_of(A, _), Prec) -->
        state(S),
        {member(error-show, S)},
        prec(A, Prec).

prec(instead_of(_, B), Prec) -->
        state(S),
        {member(error-fix, S)},
        prec(B, Prec).

mathml(omit_left(quote(Expr)), M) -->
        state(S),
        { member(error-highlight, S),
          compound(Expr),
          compound_name_arguments(Expr, Op, [L, R]) }, 
        mathml([underbrace([L, Op], "omitted"), R], M).

mathml(omit_left(quote(Expr)), M) -->
        state(S),
        { member(error-show, S),
          compound(Expr),
          compound_name_arguments(Expr, Op, [L, R]) }, 
        mathml([cancel([L, Op]), R], M).

mathml(omit_left(quote(Expr)), M) -->
        state(S),
        { member(error-fix, S),
          compound(Expr),
          compound_name_arguments(Expr, Op, [L, R]) }, 
        mathml([green([L, Op]), R], M).

paren(omit_left(quote(Expr)), Paren) -->
        paren(Expr, Paren).

prec(omit_left(quote(Expr)), Prec) -->
        prec(Expr, Prec).
        
mathml(omit_right(quote(Expr)), M) -->
        state(S),
        { member(error-highlight, S),
          compound(Expr),
          compound_name_arguments(Expr, Op, [L, R]) }, 
        mathml([L, underbrace([Op, R], "omitted")], M).

mathml(omit_right(quote(Expr)), M) -->
        state(S),
        { member(error-show, S),
          compound(Expr),
          compound_name_arguments(Expr, Op, [L, R]) }, 
        mathml([L, cancel([Op, R])], M).

mathml(omit_right(quote(Expr)), M) -->
        state(S),
        { member(error-fix, S),
          compound(Expr),
          compound_name_arguments(Expr, Op, [L, R]) }, 
        mathml([L, green([Op, R])], M).

paren(omit_right(quote(Expr)), Paren) -->
        paren(Expr, Paren).

prec(omit_right(quote(Expr)), Prec) -->
        prec(Expr, Prec).

mathml(invent_left(quote(Expr)), M) -->
        state(S),
        { member(error-highlight, S),
          compound(Expr),
          compound_name_arguments(Expr, Op, [L, R]) }, 
        mathml([underbrace([L, Op], "invented"), R], M).

mathml(invent_left(quote(Expr)), M) -->
        state(S),
        { member(error-show, S),
          compound(Expr),
          compound_name_arguments(Expr, Op, [L, R]) }, 
        mathml([red([L, Op]), R], M).

mathml(invent_left(quote(Expr)), M) -->
        state(S),
        { member(error-fix, S),
          compound(Expr),
          compound_name_arguments(Expr, Op, [L, R]) }, 
        mathml([cancel([L, Op]), R], M).

paren(invent_left(quote(Expr)), Paren) -->
        paren(Expr, Paren).

prec(invent_left(quote(Expr)), Prec) -->
        prec(Expr, Prec).

mathml(invent_right(quote(Expr)), M) -->
        state(S),
        { member(error-highlight, S),
          compound(Expr),
          compound_name_arguments(Expr, Op, [L, R]) }, 
        mathml([L, underbrace([Op, R], "invented")], M).

mathml(invent_right(quote(Expr)), M) -->
        state(S),
        { member(error-show, S),
          compound(Expr),
          compound_name_arguments(Expr, Op, [L, R]) }, 
        mathml([L, red([Op, R])], M).

mathml(invent_right(quote(Expr)), M) -->
        state(S),
        { member(error-fix, S),
          compound(Expr),
          compound_name_arguments(Expr, Op, [L, R]) }, 
        mathml([L, cancel([Op, R])], M).

paren(invent_right(quote(Expr)), Paren) -->
        paren(Expr, Paren).

prec(invent_right(quote(Expr)), Prec) -->
        prec(Expr, Prec).
        
math(expert(A = B), A -> B) --> [].
math(buggy(A \= B, _Bug), A ~> B) --> [].

example :- example([error-highlight], instead_of(sigma, s)).
example :- example([error-fix], instead_of(sigma, s)).
example :- example([error-show], instead_of(sigma, s)).

%
% Abbreviations
%
mathml(denoting(A, _, _), M) -->
        mathml(A, M).

paren(denoting(A, _, _), Paren) -->
        paren(A, Paren).

prec(denoting(A, _, _), Paren) -->
        prec(A, Paren).

example :- example(denoting(s, t + u, "something")).
example :- example(a + denoting(s, t + u, "something")).

% Collect abbreviations
abbrev(A, W) -->
        denoting0(A, X),
        {list_to_set(X, W)}.

denoting0(A, []) -->
        {atomic(A)}.

denoting0(denoting(Abbrev, Exp, Desc), W) -->
        !, denoting0(Exp, T),
        {W = [denoting(Abbrev, Exp, Desc) | T]}.

denoting0(instead_of(A, B, _), W) -->
        state(S),
        {member(error-highlight, S)},
        !, denoting0(A, X),
        denoting0(B, Y),
        {append(X, Y, W)}.

denoting0(instead_of(A, _), W) -->
        state(S),
        {member(error-show, S)},
        !, denoting0(A, W).

denoting0(instead_of(_, B), W) -->
        state(S),
        {member(error-fix, S)},
        !, denoting0(B, W).

denoting0(Comp, With) -->
    state(S, New),
    {compound(Comp),
     compound_name_arguments(Comp, _, Args)},
    {maplist({S, New}/[A, W] >> denoting0(A, W, [S], [New]), Args, Withs),
     append(Withs, With)}.

% Render abbreviations
denoting(A, []) -->
        abbrev(A, []),
        !.

denoting(A, [M]) -->
        abbrev(A, [denoting(X, Exp, Des)]),
        !, mathml([' ', "with", ' ', X = Exp, ' ', "denoting", ' ', Des, "."], M).

denoting(A, [M | MT]) -->
        abbrev(A, [denoting(X, Exp, Des) | T]),
        mathml(["with", ' ', X = Exp, ' ', "denoting", ' ', Des, ",", ' '], M),
        denoting_and(T, MT).

denoting_and([], [X]) -->
        mathml(".", X).

denoting_and([denoting(X, Exp, Des) | T], [M | MT]) -->
        mathml(["and", ' ', X = Exp, ' ', "denoting", ' ', Des], M),
        denoting_and(T, MT).

%
% Operators
%
math(A '_' B ^ C, subsup(A, B, C)) --> [].

mathml(subsup(A, B, C), msubsup([X, Y, Z])) -->
    prec(subsup(A, B, C), Prec),
    prec(A, Inner),
    (   {Prec =< Inner} -> mathml(paren(A), X) ; mathml(A, X) ),
    mathml(B, Y),
    mathml(C, Z).

paren(subsup(A, _, _), Paren) -->
    paren(A, Paren).

prec(subsup(_, _, _), Prec) -->
    prec(x^y, Prec).

math(A '_' B, sub(A, B)) --> [].

mathml(sub(A, B), msub([X, Y])) -->
    prec(sub(A, B), Prec),
    prec(A, Inner),
    (   {Prec =< Inner} -> mathml(paren(A), X) ; mathml(A, X) ),
    mathml(B, Y).

paren(sub(A, _), Paren) -->
    paren(A, Paren).

prec(sub(_, _), Prec) -->
    prec(x^y, Prec).

math(A^B, sup(A, B)) --> [].

% experimental: sin^2 x for "simple" x
mathml(sup(Sin, X), M) -->
    prec(Sin, Prec),
    prec(sin(x), Prec),
    paren(X, 0),
    prec(X, 0),
    !, state(S, [replace(Sin^X, Sin) | S]),
    mathml(Sin, M).

mathml(sup(A, B), msup([X, Y])) -->
    prec(sup(A, B), Prec),
    prec(A, Inner),
    (   {Prec =< Inner} -> mathml(paren(A), X) ; mathml(A, X) ),
    mathml(B, Y).

paren(sup(A, _), Paren) -->
    paren(A, Paren).

prec(sup(_, _), Prec) -->
    {current_op(P, xfy, ^), Prec = P}.

% Omit multiplication sign in "simple" products
math(A * B, M) -->
    paren(A / x, 0),
    !, {M = A invisible_times B}.

% Negative sign has same precedence as binary minus
math(-A, operator(Prec, fx, -, A)) -->
    prec(a-b, Prec).

% Prefix and postfix operators (e.g., factorial)
math(Comp, operator(Prec, Fix, Op, A)) -->
    {compound(Comp),
     compound_name_arguments(Comp, Op, [A]),
     current_op(P, Fix, Op), Prec = P,
     member(Fix, [xf, yf, fx, fy])}.

mathml(operator(Prec, fx, Op, A), mrow([F, X])) -->
    mathml(Op, F),
    prec(A, Inner),
    (   {Prec =< Inner} -> mathml(paren(A), X) ; mathml(A, X) ).

mathml(operator(Prec, fy, Op, A), mrow([F, Y])) -->
    mathml(Op, F),
    prec(A, Inner),
    (   {Prec < Inner} -> mathml(paren(A), Y) ; mathml(A, Y) ).

mathml(operator(Prec, xf, Op, A), mrow([X, F])) -->
    mathml(Op, F),
    prec(A, Inner),
    (   {Prec =< Inner} -> mathml(paren(A), X) ; mathml(A, X) ).

mathml(operator(Prec, yf, Op, A), mrow([Y, F])) -->
    mathml(Op, F),
    prec(A, Inner),
    (   {Prec < Inner} -> mathml(paren(A), Y) ; mathml(A, Y) ).

paren(operator(Prec, Fix, _, A), Paren) -->
    {member(Fix, [xf, fx])},
    paren(A, P),
    prec(A, Inner),
    (   {Prec =< Inner -> Paren is P + 1 ; Paren = P}).

paren(operator(Prec, Fix, _, A), Paren) -->
    {member(Fix, [yf, fy])},
    paren(A, P),
    prec(A, Inner),
    (   {Prec < Inner -> Paren is P + 1 ; Paren = P}).

prec(operator(Prec, _, _, _), Prec) --> [].

math(Comp, operator(Prec, Fix, Op, A, B)) -->
    {compound(Comp),
     compound_name_arguments(Comp, Op, [A, B]),
     current_op(P, Fix, Op), Prec = P,
     member(Fix, [xfx, yfx, xfy])}.

mathml(operator(Prec, xfx, Op, A, B), mrow([X, F, Y])) -->
    mathml(Op, F),
    prec(A, PrecA),
    (   {Prec =< PrecA} -> mathml(paren(A), X) ; mathml(A, X) ),
    prec(B, PrecB),
    (   {Prec =< PrecB} -> mathml(paren(B), Y) ; mathml(B, Y) ).

mathml(operator(Prec, xfy, Op, A, B), mrow([X, F, Y])) -->
    mathml(Op, F),
    prec(A, PrecA),
    (   {Prec =< PrecA} -> mathml(paren(A), X) ; mathml(A, X) ),
    prec(B, PrecB),
    (   {Prec < PrecB} -> mathml(paren(B), Y) ; mathml(B, Y) ).

mathml(operator(Prec, yfx, Op, A, B), mrow([Y, F, X])) -->
    mathml(Op, F),
    prec(A, PrecA),
    (   {Prec < PrecA} -> mathml(paren(A), Y) ; mathml(A, Y) ),
    prec(B, PrecB),
    (   {Prec =< PrecB} -> mathml(paren(B), X) ; mathml(B, X) ).

paren(operator(Prec, xfx, _, A, B), Paren) -->
    paren(A, PA),
    prec(A, PrecA),
    (   {Prec =< PrecA -> ParenA is PA + 1 ; ParenA = PA} ),
    paren(B, PB),
    prec(B, PrecB),
    (   {Prec =< PrecB -> ParenB is PB + 1 ; ParenB = PB} ),
    {Paren is max(ParenA, ParenB)}.

paren(operator(Prec, yfx, _, A, B), Paren) -->
    paren(A, PA),
    prec(A, PrecA),
    (   {Prec < PrecA -> ParenA is PA + 1 ; ParenA = PA} ),
    paren(B, PB),
    prec(B, PrecB),
    (   {Prec =< PrecB -> ParenB is PB + 1 ; ParenB = PB} ),
    {Paren is max(ParenA, ParenB)}.

paren(operator(Prec, xfy, _, A, B), Paren) -->
    paren(A, PA),
    prec(A, PrecA),
    (   {Prec =< PrecA -> ParenA is PA + 1 ; ParenA = PA} ),
    paren(B, PB),
    prec(B, PrecB),
    (   {Prec < PrecB -> ParenB is PB + 1 ; ParenB = PB} ),
    {Paren is max(ParenA, ParenB)}.

prec(operator(Prec, _, _, _, _), Prec) --> [].

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
is_positive(A) --> positive(A, _).
positive(A, mn(N)) --> 
    {number(A), A >= 0}, 
    state(S), {member(round3, S)},
    !, {format(atom(N), '~3f', A)}.

positive(A, mn(N)) --> 
    {number(A), A >= 0}, 
    state(S), {member(round2, S)},
    !, {format(atom(N), '~2f', A)}.

positive(A, mn(N)) --> 
    {number(A), A >= 0}, 
    state(S), {member(round1, S)},
    !, {format(atom(N), '~1f', A)}.

positive(A, mn(N)) --> 
    {number(A), A >= 0}, 
    state(S), {member(round0, S)},
    !, {format(atom(N), '~0f', A)}.

positive(A, mn(N)) --> 
    {number(A), A >= 0}, 
    state(S), {member(round, S)},
    !, {format(atom(N), '~g', A)}.

positive(A, mn(A)) --> 
    {number(A), A >= 0}.

positive(A, X) -->
    {string(A), number_string(N, A)},
    positive(N, X).

mathml(A, M) -->
    is_positive(A),
    positive(A, M).

paren(A, 0) -->
    is_positive(A).

prec(A, 0) -->
    is_positive(A).

is_negative(A) --> negative(A, _).
negative(A, X) --> 
    {number(A), A < 0, P is abs(A)},
    mathml(-P, X).

negative(A, X) --> 
    {string(A), number_string(N, A)},
    negative(N, X).

mathml(A, M) -->
    is_negative(A),
    negative(A, M).

paren(A, 0) -->
    is_negative(A).

prec(A, Prec) -->
    is_negative(A),
    prec(-a, Prec).

% Force rendering string as number
mathml(number(A), mn(A)) -->
    { number_string(N, A), N >= 0 }.

mathml(number(A), X) -->
    { quantity(A, N, Options), N < 0,
      Abs is abs(N),
      quantity(AX, Abs, Options) 
    }, mathml([-, number(AX)], X).

paren(number(A), Paren) -->
    { term_string(T, A) },
    paren(T, Paren).

prec(number(A), Prec) -->
    { term_string(T, A) },
    prec(T, Prec).

% SWISH sandbox restrictions do not allow
% a general "round", but it is not needed
% anyway.
mathml(round(A), X) -->
    state(S, [round | S]),
    mathml(A, X),
    state([round | S], S).

paren(round(A), Paren) -->
    paren(A, Paren).

prec(round(A), Prec) -->
    prec(A, Prec).

mathml(round0(A), X) -->
    state(S, [round0 | S]),
    mathml(A, X),
    state([round0 | S], S).

paren(round0(A), Paren) -->
    paren(A, Paren).

prec(round0(A), Prec) -->
    prec(A, Prec).

mathml(round1(A), X) -->
    state(S, [round1 | S]),
    mathml(A, X),
    state([round1 | S], S).

paren(round1(A), Paren) -->
    paren(A, Paren).

prec(round1(A), Prec) -->
    prec(A, Prec).

mathml(round2(A), X) -->
    state(S, [round2 | S]),
    mathml(A, X),
    state([round2 | S], S).

paren(round2(A), Paren) -->
    paren(A, Paren).

prec(round2(A), Prec) -->
    prec(A, Prec).

mathml(round3(A), X) -->
    state(S, [round3 | S]),
    mathml(A, X),
    state([round3 | S], S).

paren(round3(A), Paren) -->
    paren(A, Paren).

prec(round3(A), Prec) -->
    prec(A, Prec).

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
is_unit(A) --> unit(A, _).
unit(A, M) --> {unit(A, M)}.

unit('%', "%").
unit('kg', "kg").

mathml(A, [Num, ' ', Unit]) -->
    {compound(A),
     compound_name_arguments(A, U, [Num])},
    is_unit(U),
    unit(U, Unit).

example :- example(10 '%').
example :- example(kg(5)).

%
% Fractions
%
mathml(frac(A, B), mfrac([X, Y])) -->
    mathml(A, X),
    mathml(B, Y).

paren(frac(_, _), 0) --> [].

prec(frac(_, _), Prec) -->
    prec(x/y, Prec).

% Large fraction
mathml(dfrac(A, B), mstyle(displaystyle(true), X)) -->
    mathml(frac(A, B), X).

paren(dfrac(A, B), Paren) -->
    paren(frac(A, B), Paren).

prec(dfrac(A, B), Prec) -->
    prec(frac(A, B), Prec).

mathml(choose(A, B), mfenced(mfrac(linethickness(0), [X, Y]))) -->
    mathml(A, X),
    mathml(B, Y).

paren(choose(_, _), 1) --> [].

prec(choose(_, _), Prec) -->
    prec(x^y, Prec).

mathml(dchoose(A, B), mfenced(mstyle(displaystyle(true),
        mfrac(linethickness(0), [X, Y])))) -->
    mathml(A, X),
    mathml(B, Y).

paren(dchoose(A, B), Paren) -->
    paren(choose(A, B), Paren).

prec(dchoose(A, B), Prec) -->
    prec(choose(A, B), Prec).

math('TTEST'(D, T0, EOT, Mu, S, S_T0, S_EOT, N),
     fun('TTEST', (D, T0, EOT, Mu, S, S_T0, S_EOT, N))) --> [].

math(tratio(X, Mu, S, N), 
    fun("paired t-test", (X, S; Mu, N))) --> [].

example :- example(frac(1.5, 2)^2).
example :- example(frac(small, small) = dfrac(large, large)).

%
% Trigonometric functions
%
math(sin(A), trig(sin, A)) --> [].
math(cos(A), trig(cos, A)) --> [].
math(tan(A), trig(tan, A)) --> [].

mathml(trig(Fun, Arg), M) -->
    paren(Arg, 0),
    prec(Arg, 0),
    !, (state(Replace, S), {select(replace(Sin^N, Sin), Replace, S)}
            -> mathml(Fun^N, F) ; mathml(Fun, F)),
    mathml(Arg, X),
    {M = mrow([F, mo(&(af)), X])}.

mathml(trig(Fun, Arg), mrow([F, mo(&(af)), X])) -->
    (   state(Replace, S), {select(replace(Sin^N, Sin), Replace, S)}
            -> mathml(Fun^N, F) ; mathml(Fun, F)),
    mathml(paren(Arg), X).

prec(trig(_, _), Prec) -->
    prec(x*y, P),
    {Prec is P - 1}.

paren(trig(_, A), Paren) -->
    prec(A, 0),
    !, paren(A, Paren).

paren(trig(_, A), Paren) -->
    paren(A, P),
    {Paren is P + 1}.

example :- example(sin(pi)).
example :- example(sin(pi/2)).
example :- example(sin(pi)^2).
example :- example(sin(pi)*cos(pi)).
example :- example(sin(a + b)).
example :- example(sin(a!)^2).

%
% Special functions
%
mathml(sum(I, From, To, A), mrow([munderover([\['&sum;'], XFrom, XTo]), X])) -->
    mathml(I = From, XFrom),
    mathml(To, XTo),
    mathml(A, X).

paren(sum(_, _, _, A), Paren) -->
    paren(A, Paren).

prec(sum(_, _, _, _), Prec) -->
    prec(x+y, Prec).

mathml(argmin(I, _, _, A), M) -->
    mathml(fun(under("argmin", I), A), M).

paren(argmin(_, _, _, A), Paren) -->
    paren(A, Paren).

prec(argmin(_, _, _, _), Prec) -->
    prec(x+y, Prec).

mathml(argmax(I, _, _, A), M) -->
    mathml(fun(under("argmax", I), A), M).

paren(argmax(_, _, _, A), Paren) -->
    paren(A, Paren).

prec(argmax(_, _, _, _), Prec) -->
    prec(x+y, Prec).

mathml(under(A, B), munder([X, Y])) -->
    mathml(A, X),
    mathml(B, Y).

paren(under(A, _), Paren) -->
    paren(A, Paren).

prec(under(A, _), Prec) -->
    prec(A, Prec).

mathml(sqrt(A), msqrt(X)) -->
    mathml(A, X).

paren(sqrt(_), 0) --> [].

prec(sqrt(_), Prec) -->
    prec(x^y, Prec).

% t-distribution
math(tt(T, DF), fun('P', (abs('T') >= T ; "df" = DF))) --> [].
math(ut(T, DF), fun('P', ('T' >= T ; "df" = DF))) --> [].
math(pt(T, DF), fun('P', ('T' =< T ; "df" = DF))) --> [].
 
math(dbinom(K, N, P), fun('P' '_' "Bi", ['X' = K ; (N, P)])) --> [].
math(pbinom(K, N, P), fun('P' '_' "Bi", ['X' =< K ; (N, P)])) --> [].
math(ubinom(K, N, P), fun('P' '_' "Bi", ['X' >= K ; (N, P)])) --> [].
math(qbinom(Alpha, N, P), fun('Q' '_' "Bi", [Alpha ; (N, P)])) --> [].
math(uqbinom(Alpha, N, P), fun('Q' '_' "Bi", [1 - Alpha ; (N, P)])) --> [].

% Bit unusual terminology
math(bernoulli(Succ, N, Pi), successes(Succ, Pi) * failures(N-Succ, Pi)) --> [].
math(successes(Succ, Pi), Pi^Succ) --> [].
math(failures(Fail, Pi), (1-Pi)^Fail) --> [].

% General functions
mathml(fun(Name, Args), mrow([N, &(af), A])) -->
    mathml(Name, N),
    mathml(paren(Args), A).

paren(fun(_, Args), Paren) -->
    paren(paren(Args), Paren).

prec(fun(_, _), Prec) -->
    prec(x^y, Prec).

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
