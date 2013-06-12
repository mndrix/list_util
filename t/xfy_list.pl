:- use_module(library(list_util)).
:- use_module(library(tap)).

forwards :-
    xfy_list(',', (a,b,[c],d), List),
    List = [a,b,[c],d].
backwards :-
    xfy_list('$', Term, [one, two, 3]),
    Term = '$'(one, '$'(two, 3)).
find_op :-
    xfy_list(Op, 4^3^2, [4,3,2]),
    Op = '^'.
