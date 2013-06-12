:- use_module(library(list_util)).
:- use_module(library(tap)).

atoms :-
    maximum([c,b,a,d], d).

empty(fail) :-
    maximum([], _).

'list unbound' :-
    maximum(List, 90),
    List = [90, 3, 7, 6, 5].

'both unbound' :-
    maximum(List, Max),
    Max = z,
    List = [z, k, r, t, v, u, j].
