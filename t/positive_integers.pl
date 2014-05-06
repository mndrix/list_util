:- use_module(library(list_util)).
:- use_module(library(tap)).

append :-
    positive_integers(Z),
    length(Xs, 10),
    append(Xs, _, Z),
    Xs = [1,2,3,4,5,6,7,8,9,10].

take :-
    positive_integers(Z),
    take(Z, 7, Xs),
    Xs = [1,2,3,4,5,6,7].
