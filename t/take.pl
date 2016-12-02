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

blank_n :-
    take(N, [1,2,3], [1]),
    N == 1.

blank_n_and_list :-
    take(N, List, [3,4,5]),
    N == 3,
    List = [3,4,5|Rest],
    var(Rest).

nondet_take :-
    findall(N-Take, take(N, [3,4,5], Take), [N0-T0, N1-T1, N2-T2, N3-T3, V-T4]),
    N0 == 0,
    N1 == 1,
    N2 == 2,
    N3 == 3,
    T0 == [],
    T1 == [3],
    T2 == [3,4],
    T3 == [3,4,5],
    T4 == [3,4,5],
    var(V).
