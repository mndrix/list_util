:- use_module(library(list_util)).

% bob
bob(b).
bob(o).
bob(b).

% xyzxyzxyzxyz...
foo(x).
foo(y).
foo(z).
foo(X) :-
    foo(X).

% throws exception on backtracking
eventually_deadly.
eventually_deadly :-
    throw(die).

:- use_module(library(tap)).

no_solutions :-
    lazy_findall(x,fail,Xs),
    Xs=[].

take_finite :-
    lazy_findall(X,bob(X),Bob),
    take(30,Bob,Letters),
    Letters = [b,o,b].

take_infinite :-
    lazy_findall(X,foo(X),Xs),
    take(4,Xs,L),
    L == [x,y,z,x].

% missing predicate fails right away since first value is eager
'no predicate: lazy'(throws(_)) :-
    lazy_findall(x,no_predicate_with_this_name,_).

'delayed exception: once is ok' :-
    lazy_findall(x,eventually_deadly,Xs),
    Xs = [_|_].

'delayed exception: twice dies'(throws(die)) :-
    lazy_findall(x,eventually_deadly,Xs),
    Xs = [_,_|_].
