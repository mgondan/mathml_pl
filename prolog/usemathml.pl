:- use_module(library(mathml)).
:- use_module(library(html_write)).

example :-
    mathml(sin(x), M),
    writeln('<HTML>'),
    print_html(M),
    writeln('</HTML>').
