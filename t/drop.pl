:- use_module(library(list_util)).
:- use_module(library(tap)).

zero :-
    drop(0, [a,b], [a,b]).
one :-
    drop(1, [a,b], [b]).
huge :-
    drop(99, [1,2,3], []).

backward :-
    drop(3, L, [4,5]),
    L = [A, B, C, 4, 5],
    var(A), var(B), var(C).
backward_zero :-
    drop(0, L, [1,2,3]),
    L = [1,2,3].
backward_huge :-
    forall(
        drop(3,L,[]),
        ( length(L,N)
        , N =< 3
        , term_variables(L, Vs)
        , length(Vs, N)
        )
    ).
