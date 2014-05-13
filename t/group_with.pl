:- use_module(library(list_util)).

type(X, Type) :- atom(X), !, Type=atom.
type(X, Type) :- integer(X), !, Type=integer.
type(X, Type) :- float(X), !, Type=float.


:- use_module(library(tap)).

by_type :-
    group_with(type, [a,b,1,c,2.0,9], Groups),
    Groups == [[a,b,c],[2.0],[1,9]].

by_length :-
    group_with(atom_length, [a,hi,bye,b], Groups),
    Groups == [[a,b],[hi],[bye]].
