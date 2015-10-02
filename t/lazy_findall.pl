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

% number of current Prolog threads
thread_count(N) :-
    bagof(x,T^Status^thread_property(T,status(Status)),Xs),
    length(Xs,N).

:- use_module(library(tap)).

no_solutions :-
    thread_count(N),
    lazy_findall(x,fail,Xs),
    Xs=[],
    thread_count(N).

take_finite :-
    thread_count(N),
    lazy_findall(X,bob(X),Bob),
    take(30,Bob,Letters),
    Letters = [b,o,b],
    thread_count(N).

take_infinite :-
    lazy_findall(X,foo(X),Xs),
    take(4,Xs,L),
    L == [x,y,z,x].

take_infinite_gc(todo) :-
    thread_count(N),
    take_infinite,  % shouldn't leave threads lying about
    thread_count(N).

% missing predicate fails right away since first value is eager
'no predicate: lazy'(throws(_)) :-
    thread_count(N),
    lazy_findall(x,no_predicate_with_this_name,_),
    thread_count(N).

'delayed exception: once is ok' :-
    lazy_findall(x,eventually_deadly,Xs),
    Xs = [_|_].

'delayed exception: twice dies'(throws(die)) :-
    thread_count(N),
    lazy_findall(x,eventually_deadly,Xs),
    Xs = [_,_|_],
    thread_count(N).
