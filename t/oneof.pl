:- use_module(library(list_util), [oneof/2]).
:- use_module(library(tap)).

vowels :-
    include(oneof([a,e,i,o,u]), [b,i,g,r,e,d,c,a,t], Out),
    Out == [i,e,a].

small_primes :-
    maplist(oneof([2,3,5,7,11]), [7,3,2]).
