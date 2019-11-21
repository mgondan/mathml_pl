:- use_module(library(dcg/basics)).

%
% General numeric input
%
% 1.5 cm
%
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
    int(I),
    dotfrac(F, DotOpt),
    power(E),
    { Number is S * (I + F) * 10^E, 
      option(dec(D), DotOpt, 0),
      Dec is D - E,
      merge_options([dec(Dec) | SgnOpt], DotOpt, Options) }.

% Sign
sgn(+1, [sgn(+)]) --> "+".
sgn(-1, [sgn(-)]) --> "-"; [226, 136, 146].
sgn( 1, []) --> "".

int(I) --> 
    digits([H | T]),
    {number_codes(N, [H | T]), I is N + 0.0}.

dotfrac(F, Options) --> dot, frac(F, Options).
dotfrac(0, []) --> [].

dot --> ".".
dot --> ",".

frac(F, [dec(Dec)]) --> digits([H | T]),
    {number_codes(N, [H | T]), length([H | T], Dec), F is N/10.0^Dec}.

% times 10^E
power(E) --> exp_e, sgn(S, _), int(C),
    {E is S * C}.
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
    C = `.5 kg`, string_codes(S, C), writeln(S),
    number(N, Options, C, []),
    writeln(N), writeln(Options).

ex :-
    C = `1.5 kg`, string_codes(S, C), writeln(S),
    number(N, Options, C, []),
    writeln(N), writeln(Options).

ex :-
    C = `15 kg`, string_codes(S, C), writeln(S),
    number(N, Options, C, []),
    writeln(N), writeln(Options).

ex :-
    C = `15 g`, string_codes(S, C), writeln(S),
    number(N, Options, C, []),
    writeln(N), writeln(Options).

ex :-
    C = `-15 g`, string_codes(S, C), writeln(S),
    number(N, Options, C, []),
    writeln(N), writeln(Options).

ex :-
    C = `-15E10 g`, string_codes(S, C), writeln(S),
    number(N, Options, C, []),
    writeln(N), writeln(Options).

ex :-
    C = `-1.5E-10 g`, string_codes(S, C), writeln(S),
    number(N, Options, C, []),
    writeln(N), writeln(Options).


ex :-
    C = `0.09`, string_codes(S, C), writeln(S),
    number(N, Options, C, []),
    writeln(N), writeln(Options).

ex :-
    C = `9%`, string_codes(S, C), writeln(S),
    number(N, Options, C, []),
    writeln(N), writeln(Options).

ex :-
    C = `9.1 %`, string_codes(S, C), writeln(S),
    number(N, Options, C, []),
    writeln(N), writeln(Options).

