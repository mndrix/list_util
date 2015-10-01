:- use_module(library(list_util)).
:- use_module(library(tap)).

strict_list :-
    lazy_maplist(plus(1), [1,1,1,1], Twos),
    var(Twos),
    Twos = [2,2,2,2].

infinite_list :-
    positive_integers(Integers),
    lazy_maplist(plus(10), Integers, TenMore),
    maplist(var, [Integers, TenMore]),
    take(7, TenMore, [11,12,13,14,15,16,17]),
    term_variables(TenMore, [Rest]),
    Rest = [18,19,20|_].
