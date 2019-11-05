:- module(
       math,
       [math/2,
        example/0,
        example/1,
        op(200, xfy, '_'),
        op(400, xf, '%'),
        op(200, xf, !)]).

:- op(200, xfy, '_').
:- op(200, xf, !).
:- op(400, xf, '%').

:- discontiguous math/2.
:- discontiguous example/1.

%
% Invoke with something like
%
% math(dbinom(k, 'N', pi) =
%     with(choose('N', k) = dfrac('N'!, k!*('N'-k)!), "the binomial coefficient")
%     * pi^k * (1-pi)^('N' - k), M).
%

%
% Check types
%
is_number(X) :-
    number(X).

is_positive(X) :-
    is_number(X),
    X >= 0.

is_negative(X) :-
    is_number(X),
    X < 0.

is_identifier(X) :-
    atom(X),
    identifier(X).

is_identifier(X) :-
    atom(X),
    \+ is_punctuation(X),
    \+ is_operator(X).

is_punctuation(X) :-
    atom(X),
    punctuation(X).

is_punctuation(&(X)) :-
    atom(X).

is_operator(X) :-
    atom(X),
    operator(X).

is_operator(X) :-
    atom(X),
    current_op(_, _, X).

is_string(X) :-
    string(X).

%
% Variables ('identifiers')
%
identifier(alpha).
identifier(mu).
identifier(pi).
identifier(sigma).

math(A, X) :-
    is_identifier(A),
    !, X = identifier(A).

example(var) :-
    A = sin(alpha),
    math(A, X),
    writeln(x = X).

%
% Punctuation
%
punctuation(' ').
punctuation(ldots).
punctuation(cdots).

math(A, X) :-
    is_punctuation(A),
    !, X = punctuation(A).

example(punct) :-
    A = list(' ', ["with", n! = 1 * cdots * n, "denoting the factorial"]),
    math(A, X),
    writeln(x = X).

%
% Operators
%
operator(*).
operator(/).
operator(=\=).
operator(=).
operator(<).
operator(>).
operator(=<).
operator(>=).
operator(\=).
operator(!).
operator('%').
operator(',').
operator(';').
operator('|').

math(A, X) :-
    is_operator(A),
    !, X = operator(A).

example(op) :-
    A = (alpha = pi/2),
    math(A, X),
    writeln(x = X).

%
% Symbols (non-italicized)
%
math(A, X) :-
    is_string(A),
    !, X = string(A).

%
% Parentheses
%
math(paren(A), P) :-
    !, math(A, X),
    P = paren('', X).

math(parenthesis(A), P) :-
    !, math(A, X),
    P = paren('(', X).

math(bracket(A), P) :-
    !, math(A, X),
    P = paren('[', X).

math(curly(A), P) :-
    !, math(A, X),
    P = paren('{', X).

example(paren) :-
    A = paren(x),
    math(A, X),
    writeln(x = X).

%
% Lists (e.g., function arguments)
%
math([H | T], X) :-
    !, math(list(',', [H | T]), X).

math(list([H | T]), X) :-
    !, math(list('', [H | T]), X).

math(list(Sep, List), X) :-
    !, math(Sep, S),
    maplist(math, List, L),
    X = list(S, L).

%
% Decorations
%
math(red(A), X) :-
    !, math(color(red, A), X).

math(green(A), X) :-
    !, math(color(green, A), X).

math(blue(A), X) :-
    !, math(color(blue, A), X).

math(black(A), X) :-
    !, math(color(black, A), X).

math(color(C, A), Y) :-
    !, math(A, X),
    Y = color(C, X).

%
% Mistakes
%
math(instead_of(A, B), Z) :-
    !, math(A, X),
    math(B, Y),
    Z = instead_of(X, Y).

%
% Abbreviations
%
math(with(A, B, C), W) :-
    !, math(A, X),
    math(B, Y),
    math(C, Z),
    W = with(X, Y, Z).

%
% Operators
%
math(A '_' B ^ C, X) :-
    !, math(subsup(A, B, C), X).

% Negative sign has same precedence as binary minus
math(-A, C) :-
    !, math(A, X),
    current_op(Prec, yfx, -),
    C = op(Prec, fy, -, X).

% Prefix and postfix operators (e.g., factorial)
math(Comp, C) :-
    compound(Comp),
    compound_name_arguments(Comp, Op, [A]),
    current_op(Prec, Fix, Op),
    member(Fix, [xf, yf, fx, fy]),
    !, math(A, X),
    C = op(Prec, Fix, Op, X).

math(Comp, C) :-
    compound(Comp),
    compound_name_arguments(Comp, Op, [A, B]),
    current_op(Prec, Fix, Op),
    member(Fix, [xfx, yfx, xfy]),
    !, math(A, X),
    math(B, Y),
    C = op(Prec, Fix, Op, X, Y).

%
% Numbers
%
math(A, X) :-
    is_positive(A),
    !, X = positive(A).

math(A, X) :-
    is_negative(A),
    !, Abs is abs(A),
    math(-Abs, X).

example(num) :-
    A = -1,
    math(A, X),
    writeln(x = X).

%
% Functions
%
% Trigonometric functions
math(sin(A), X) :-
    !, math(trig(sin, A), X).

math(cos(A), X) :-
    !, math(trig(cos, A), X).

math(tan(A), X) :-
    !, math(trig(tan, A), X).

math(trig(Name, Arg), X) :-
    !, math(Name, N),
    math(Arg, A),
    X = trig(N, A).

% Some macros
math(dbinom(K, N, P), X) :-
    !, math(fun(dbinom, K ; [N, P]), X).

math(pbinom(K, N, P), X) :-
    !, math(fun(pbinom, K ; [N, P]), X).

math(ubinom(K, N, P), X) :-
    !, math(fun(ubinom, K ; [N, P]), X).

math(qbinom(Alpha, N, P), X) :-
    !, math(fun(qbinom, Alpha ; [N, P]), X).

math(uqbinom(Alpha, N, P), X) :-
    !, math(fun(uqbinom, Alpha ; [N, P]), X).

% Bit unusual terminology
math(bernoulli(Succ, N, Pi), X) :-
    !, math(successes(Succ, Pi) * failures(N-Succ, Pi), X).

math(successes(Succ, Pi), X) :-
    !, math(Succ^Pi, X).

math(failures(Fail, Pi), X) :-
    !, math(Fail^(1-Pi), X).

% General compounds
math(Comp, X) :-
    compound(Comp),
    !, compound_name_arguments(Comp, Name, Args),
    math(Name, N),
    maplist(math, Args, A),
    X = fun(N, A).

example(fun) :-
    A = sqrt(2),
    math(A, X),
    writeln(x = X).

example(fun) :-
    A = dbinom(k, n, pi),
    math(A, X),
    writeln(math = A),
    writeln(rep = X).

% invoke all examples
example :-
    findall(true, example(_), _).







