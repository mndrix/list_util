:- use_module(library(list_util)).

even(X) :- 0 is X mod 2.

:- use_module(library(tap)).

'strict lists' :-
    lazy_include(even, [1,2,3,4,5], Xs),
    var(Xs), % remains a variable
    Xs = [2,4].

'infinite lists' :-
    positive_integers(Z),
    lazy_include(even, Z, Xs),
    var(Xs),  % remains a variable until needed
    Xs = [2|Rest],
    var(Rest), % remains a variable until needed
    Rest = [4,6,8,10,12,14|_].
