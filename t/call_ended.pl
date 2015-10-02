:- use_module(library(list_util),[]).

a(x).
a(y).
a(z).

b(x).
b(_) :- fail.

c(x).
c(_) :- throw(die).

:- use_module(library(tap)).

zero :-
    list_util:call_ended(fail,End),
    End == fail.

one :-
    list_util:call_ended(X=7,End),
    End == done,
    X == 7.

exception :-
    list_util:call_ended(throw(die),End),
    End == exception(die).

many :-
    bagof(End-X,list_util:call_ended(a(X),End),Endings),
    Endings == [
        more-x,
        more-y,
        done-z
    ].

spurious :-
    Count = n(first),
    list_util:call_ended(b(X),End),
    ( Count=n(first), X==x, End==more ->
        nb_setarg(1,Count,second),
        fail
    ; Count=n(second), var(X), End==fail ->
        true
    ; otherwise ->
        throw(unexpected_outcome(spurious,X,End))
    ).

once_then_die :-
    Count = n(first),
    list_util:call_ended(c(X),End),
    ( Count=n(first), X==x, End==more ->
        nb_setarg(1,Count,second),
        fail
    ; Count=n(second), var(X), End==exception(die) ->
        true
    ; otherwise ->
        throw(unexpected_outcome(once_then_die,X,End))
    ).
