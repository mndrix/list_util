:- use_module(library(list_util)).

incr(A,B,A) :-
    B is A + 1.

add(A-B,B-C,A) :-
   C is A + B.

failure(_,_,_) :-
    fail.

few(A,B,A) :-
    A < 5,
    B is A + 1.

:- use_module(library(tap)).

integers :-
    iterate(incr, 1, Ints),
    length(Xs, 10),
    append(Xs, _, Ints),
    Xs = [1,2,3,4,5,6,7,8,9,10].

fibonacci :-
    iterate(add, 1-1, Fibs),
    length(Xs, 9),
    append(Xs, _, Fibs),
    Xs = [1,1,2,3,5,8,13,21,34].

empty_list :-
    iterate(failure, x, List),
    List = [].

just_a_few :-
    iterate(few, 1, List),
    List = [1,2,3,4].
