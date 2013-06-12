:- use_module(library(list_util)).
:- use_module(library(tap)).

n_huge :-
    take([a,b], 100, [a,b]).
n_huge_empty :-
    take([], 100, []).
n_small :-
    take([a,b,c,d], 2, [a,b]).
none :-
    take([a,b,c], 0, []).
none_empty :-
    take([], 0, []).

backward :-
    take(L, 3, [a,b,c]),
    L = [a,b,c|T],
    var(T).
backward_huge :-
    take(L, 300, [a,b]),
    L = [a,b].
