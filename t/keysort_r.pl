:- use_module(library(list_util)).
:- use_module(library(tap)).

atom_int :-
    keysort_r([a-1,c-3,b-2],[c-3,b-2,a-1]).
mixed :-
    keysort_r([beta-2,9-nine,7.4-float], [beta-2,9-nine,7.4-float]).
