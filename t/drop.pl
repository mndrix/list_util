:- use_module(library(list_util)).
:- use_module(library(tap)).

zero :-
    drop([a,b], 0, [a,b]).
one :-
    drop([a,b], 1, [b]).
huge :-
    drop([1,2,3], 99, []).

backward :-
    drop(L, 3, [4,5]),
    L = [A, B, C, 4, 5],
    var(A), var(B), var(C).
backward_zero :-
    drop(L, 0, [1,2,3]),
    L = [1,2,3].
backward_huge :-
    forall(
        drop(L,3,[]),
        ( length(L,N)
        , N =< 3
        , term_variables(L, Vs)
        , length(Vs, N)
        )
    ).
