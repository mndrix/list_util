:- use_module(library(list_util)).

prime(1).
prime(2).
prime(3).
prime(5).
prime(7).

composite(X) :-
    \+ prime(X).

divisibility(<, X, Y) :-
    prime(X),
    composite(Y).

divisibility(>, X, Y) :-
    composite(X),
    prime(Y).

divisibility(=, X, Y) :-
    maplist(prime, [X, Y]).

divisibility(=, X, Y) :-
    maplist(composite, [X, Y]).

:- use_module(library(tap)).

none :-
    group_by(compare, [], []).

one :-
    group_by(compare, [a], [[a]]).

several :-
    group_by(compare, [a,b,c,b,a,b,3,q,c,1], [[a,a],[b,b,b],[c,c],[3],[q],[1]]).

primes_and_composites :-
    group_by(divisibility, [1,2,3,4,5,6,7], [[1,2,3,5,7],[4,6]]).

primes_and_composites_instantiation_error :-
    catch(
        (  group_by(divisibility, X, [[1,2,3,5,7],[4,6]]),
           fail
        ),
        Error,
        Error = error(instantiation_error, X)
    ).
