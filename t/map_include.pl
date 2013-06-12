:- use_module(library(list_util)).

% predicates for use during testing
map_include_test_num(X,X) :- number(X).
map_include_test_f(X,Y) :- number(X), succ(X, Y).

:- use_module(library(tap)).

just_mapping :-
    map_include(succ, [1,2,3], [2,3,4]).
just_filtering :-
    map_include(map_include_test_num, [1,a,3], [1,3]).
both_mapping_and_filtering :-
    map_include(map_include_test_f, [a,1,2,b], [2,3]).
