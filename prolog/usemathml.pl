:- use_module(library(mathml)).
:- use_module(library(html_write)).

example :-
    mathml(sin(x), M), 
    html(html(math(M)), Tokens, []),
    print_html(Tokens).
