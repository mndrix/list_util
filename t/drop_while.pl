:- use_module(library(list_util)).

no(_) :- fail.
yes(_) :- true.
even(X) :- 0 is X mod 2.

:- use_module(library(tap)).

never :-
    drop_while(no, [a,b,c], List),
    List == [a,b,c].

always :-
    drop_while(yes, [john,sue,alice], List),
    List == [].

evens :-
    drop_while(even, [2,4,6,9,12], List),
    List == [9,12].
