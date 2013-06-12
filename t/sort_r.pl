:- use_module(library(list_util)).
:- use_module(library(tap)).

atoms :-
    sort_r([a,c,j,b], [j,c,b,a]).
duplicates :-
    sort_r([3,m,4,1,m,9], [m,9,4,3,1]).
