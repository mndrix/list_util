:- use_module(library(list_util)).

prime(1).
prime(2).
prime(3).
prime(5).
prime(7).

composite(X) :-
    \+ prime(X).

divisibility(X, Y) :-
    maplist(prime, [X, Y]).

divisibility(X, Y) :-
    maplist(composite, [X, Y]).

:- use_module(library(tap)).

none :-
    group_by(==, [], []),
    group([], []).

none_backwards :-
    group_by(==, X, []),
    X == [],
    group(Y, []),
    Y == X.

one :-
    group_by(==, [a], [[a]]),
    group([a], [[a]]).

one_backwards :-
    group_by(==, X, [[a]]),
    X == [a],
    group(Y, [[a]]),
    Y == X.

several :-
    group_by(==, [m,i,s,s,i,s,s,i,p,p,i], [[m],[i],[s,s],[i],[s,s],[i],[p,p],[i]]),
    group([m,i,s,s,i,s,s,i,p,p,i], [[m],[i],[s,s],[i],[s,s],[i],[p,p],[i]]).

several_backwards :-
    group_by(==, X, [[m],[i],[s,s],[i],[s,s],[i],[p,p],[i]]),
    X == [m,i,s,s,i,s,s,i,p,p,i],
    group(Y, [[m],[i],[s,s],[i],[s,s],[i],[p,p],[i]]),
    Y == X.

primes_and_composites :-
    group_by(divisibility, [1,2,3,4,5,6,7], [[1,2,3],[4],[5],[6],[7]]).

primes_and_composites_instantiation_error :-
    catch(
        (  group_by(divisibility, X, _Y),
           fail
        ),
        Error,
        Error = error(instantiation_error, X)
    ).
