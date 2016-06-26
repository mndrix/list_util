:- use_module(library(list_util)).

% predicates for use during testing
map_include_test_num(X,X) :- number(X).
map_include_test_num_2(X,X,X) :- number(X).
map_include_test_num_3(X,X,X,X) :- number(X).
map_include_mapping_3(A,_B,C,D) :- D is C+A.
map_include_test_f(X,Y) :- number(X), succ(X, Y).
map_include_test_f_2(X,Y,Z) :- maplist(number, [X,Y]), Z is X+Y.
map_include_test_f_3(A,B,C,D) :- maplist(number,[A,B,C]), D is A*B*C.

:- use_module(library(tap)).

just_mapping :-
    map_include(succ, [1,2,3], [2,3,4]),
    map_include(plus, [1,2,3], [1,2,3], [2,4,6]),
    map_include(map_include_mapping_3, [5,34,12], [15,44,22], [3,1,22], [8,35,34]).
just_filtering :-
    map_include(map_include_test_num, [1,a,3], [1,3]),
    map_include(map_include_test_num_2, [a,2,q], [a,2,q], [2]),
    map_include(map_include_test_num_3, [1,a,3], [1,5,3], [1,2,3], [1,3]).
both_mapping_and_filtering :-
    map_include(map_include_test_f, [a,1,2,b], [2,3]),
    map_include(map_include_test_f_2, [a,1,2], [1,3,4], [4,6]),
    map_include(map_include_test_f_3, [a,1,2], [1,3,4], [5,10,15], [30,120]).
