:- use_module(library(dcg/basics)).

%
% General numeric input
%
% 1.5 cm
%
string_number(Number, String, Options) :-
    string_codes(String, Codes), 
    number(Number, Options, Codes, []).

number(Number, Options) --> 
    numb(N, NumOpt),
    blanks,
    unit(_U, UnitOpt),
    { option(fac(M), UnitOpt, 1),
      Number is N * M, 
      option(dec(D), NumOpt, 0),
      Dec is D - log10(M),
      merge_options([dec(Dec) | UnitOpt], NumOpt, Options)
    }.

% Real number
numb(Number, Options) -->
    sgn(S, SgnOpt),
    int(I, IntOpt),
    dotfrac(F, DotOpt),
    { \+ option(int(empty), IntOpt); 
      \+ option(dec(0), DotOpt),
      merge_options(IntOpt, DotOpt, IntDotOpt)
    },
    power(E),
    { Number is S * (I + F) * 10^E, 
      option(dec(D), DotOpt, 0),
      Dec is D - E,
      merge_options([dec(Dec) | SgnOpt], IntDotOpt, Options) 
    }.

% Sign
sgn(+1, [sgn(+)]) --> "+".
sgn(-1, [sgn(-)]) --> "-"; [226, 136, 146].
sgn( 1, []) --> "".

int(I, []) --> 
    digits([H | T]),
    {number_codes(N, [H | T]), I is N + 0.0}.

int(0, [int(empty)]) --> "".

dotfrac(F, Options) --> dot, frac(F, Options).
dotfrac(0, [dot(''), dec(0)]) --> [].

dot --> ".".
dot --> ",".

frac(F, [dec(Dec)]) --> digits([H | T]),
    {number_codes(N, [H | T]), length([H | T], Dec), F is N/10.0^Dec}.

% times 10^E
power(E) --> exp_e, sgn(S, _), int(C, IntOpt),
    {\+ option(int(empty), IntOpt),
      E is S * C}.
power(0) --> [].

exp_e --> "E".
exp_e --> "e".
exp_e --> blanks, "*", blanks, "10", blanks, "^", blanks.
exp_e --> blanks, "*", blanks, "10", blanks, "**", blanks.
exp_e --> blanks, [195, 151], blanks, "10", blanks, "^", blanks.
exp_e --> blanks, [195, 151], blanks, "10", blanks, "**", blanks.

unit('', []) --> "".
unit('%', [fac(0.01), si('%')]) --> "%".

unit(U, [si(SI) | Options]) --> 
    modifier(M, Options), 
    si(SI),
    { string_concat(M, SI, U)}.

% Modifier
modifier(k, [fac(1000)]) --> "k".
modifier(c, [fac(0.01)]) --> "c".
modifier(m, [fac(0.001)]) --> "m".
modifier('', [fac(1)]) --> "".

% Unit
si(g) --> "g".
si(m) --> "m".

ex :-
    S = ".5 kg", writeln(S),
    string_number(N, S, Options),
    writeln(N), writeln(Options).

ex :-
    S = "1.5 kg", writeln(S),
    string_number(N, S, Options),
    writeln(N), writeln(Options).

ex :-
    S = "15 kg", writeln(S),
    string_number(N, S, Options),
    writeln(N), writeln(Options).

ex :-
    S = "15 g", writeln(S),
    string_number(N, S, Options),
    writeln(N), writeln(Options).

ex :-
    S = "-15 g", writeln(S),
    string_number(N, S, Options),
    writeln(N), writeln(Options).

ex :-
    S = "-15E10 g", writeln(S),
    string_number(N, S, Options),
    writeln(N), writeln(Options).

ex :-
    S = "-1.5E-10 g", writeln(S),
    string_number(N, S, Options),
    writeln(N), writeln(Options).

ex :-
    S = "0.09", writeln(S),
    string_number(N, S, Options),
    writeln(N), writeln(Options).

ex :-
    S = "9%", writeln(S),
    string_number(N, S, Options),
    writeln(N), writeln(Options).

ex :-
    S = "9.1 %", writeln(S),
    string_number(N, S, Options),
    writeln(N), writeln(Options).
