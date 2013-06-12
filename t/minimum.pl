:- use_module(library(list_util)).
:- use_module(library(tap)).

atoms :-
    minimum([c,b,a,d], a).

empty(fail) :-
    minimum([], _).

'list unbound' :-
    minimum(List, 3),
    List = [9, 3, 7, 6, 5].

'both unbound' :-
    minimum(List, Minimum),
    Minimum = j,
    List = [z, k, r, t, v, u, j].
