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

:- use_module(library(tap)).

take_finite :-
    lazy_findall(X,bob(X),Bob),
    take(30,Bob,Letters),
    Letters = [b,o,b].

take_infinite :-
    lazy_findall(X,foo(X),Xs),
    take(4,Xs,L),
    L == [x,y,z,x].
