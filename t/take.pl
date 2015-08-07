:- use_module(library(list_util)).
:- use_module(library(tap)).

n_huge :-
    take(100, [a,b], [a,b]).
n_huge_empty :-
    take(100, [], []).
n_small :-
    take(2, [a,b,c,d], [a,b]).
none :-
    take(0, [a,b,c], []).
none_empty :-
    take(0, [], []).

backward :-
    take(3, L, [a,b,c]),
    L = [a,b,c|T],
    var(T).
backward_huge :-
    take(300, L, [a,b]),
    L = [a,b].
