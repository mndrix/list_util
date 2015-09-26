:- use_module(library(list_util)).
:- use_module(library(solution_sequences), [limit/2]).

generate_ok(N,Xs) :-
    % correct length
    length(Xs,N),

    % just variables
    maplist(var,Xs),

    % all the same variable
    term_variables(Xs,Vars),
    ( N=0 -> Vars=[]; Vars=[_]).


:- use_module(library(tap)).

forward :-
    replicate(4, q, Xs),
    Xs == [q, q, q, q].


backward :-
    replicate(N, X, [1,1]),
    N == 2,
    X == 1.


empty :-
    replicate(0, ab, L),
    L == [].


generate :-
    forall(
        limit(6,replicate(N,_,Xs)),
        generate_ok(N,Xs)
    ).
