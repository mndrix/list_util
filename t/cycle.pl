:- use_module(library(list_util)).
:- use_module(library(tap)).


bound_drop :-
    cycle([a,b,c,d], Xs),
    drop(5, Xs, Take),
    Xs == [a,b,c,d,a|Take].

bound_take :-
    cycle([0,0,1], Xs),
    take(6, Xs, [0,0,1,0,0,1]),
    term_variables(Xs, [Rest]),
    Xs == [0,0,1,0,0,1|Rest],
    var(Rest).

unbound_drop :-
    cycle([A,B,C], Xs),
    drop(2, Xs, Take),
    maplist(var, [A,B,C,Take]),
    Xs == [A,B|Take].

unbound_take :-
    cycle([A,B,A], Xs),
    take(4, Xs, Take),
    maplist(var, [A,B]),
    term_variables(Xs, [A,B,_]),
    Take == [A,B,A,A].

empty :-
    cycle([],[]).

% I wish this test could pass.  Unfortunately, iterate/3 cuts choicepoints in
% its goal.  That prevents cycle/2 from backtracking to solve for Sequence.
% If there's a clean way to make it work, that'd be great to do someday.
sequence_detection(todo) :-
    cycle(Sequence,[a,b,c,a,b,c]),
    Sequence == [a,b,c].
