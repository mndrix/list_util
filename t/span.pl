:- use_module(library(list_util)).

no(_) :- fail.
yes(_) :- true.
even(X) :- 0 is X mod 2.

:- use_module(library(tap)).

never :-
    span(no, [a,b,c], Prefix, Suffix),
    Prefix == [],
    Suffix == [a,b,c].

always :-
    span(yes, [john,sue,alice], Prefix, Suffix),
    Prefix == [john,sue,alice],
    Suffix == [].

evens :-
    span(even, [2,4,6,9,12], Prefix, Suffix),
    Prefix == [2,4,6],
    Suffix == [9,12].
