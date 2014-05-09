:- use_module(library(list_util)).

no(_) :- fail.
yes(_) :- true.
even(X) :- 0 is X mod 2.

:- use_module(library(tap)).

never :-
    take_while(no, [a,b,c], List),
    List == [].

always :-
    take_while(yes, [john,sue,alice], List),
    List == [john,sue,alice].

evens :-
    take_while(even, [2,4,6,9,12], List),
    List == [2,4,6].
