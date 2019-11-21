:- use_module(library(dcg/high_order)).

table(Head, Body) -->
    html(table(class(table), [\thead(Head), \tbody(Body)])).

thead(Head) -->
    html(thead(tr(\foreach(member(X, Head), html(th(X)))))).
               
tbody(Body) -->
    html(tbody(\foreach(member(X, Body), html(\trow(X))))).

trow([H | Row]) -->
    html(tr([th(H), \foreach(member(X, Row), html(td(X)))])).

response(Id) -->
    html(
        div(class("panel panel-default"),
            div(class("panel-body"),
                div(class("form-group"),
                    [ label("Response"),
                      div(class("input-group"), 
                          [ input([class("form-control"), id(Id), name(Id)]),
                            div(class("input-group-btn"),
                                button([class("btn btn-default"), type(submit), id(submit)], "Submit"))])])))).
