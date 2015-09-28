:- use_module(library(list_util)).
:- use_module(library(tap)).


bound :-
    repeat(r, Rs),
    take(7, Rs, Repeats),
    length(Repeats, 7),
    maplist(==(r), Repeats).

unbound :-
    repeat(X, Rs),
    take(4, Rs, Repeats),
    length(Repeats, 4),
    term_variables(Repeats,[X]),
    maplist(var, Repeats).
