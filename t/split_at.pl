:- use_module(library(list_util)).
:- use_module(library(tap)).


zero :-
  split_at(0, [a,b], [], [a,b]).

zero_empty :-
  split_at(0, [], [], []).

n_small :-
  split_at(3, [a,b,c,d,e,f], [a,b,c], [d,e,f]).

n_huge :-
  split_at(1000, [a,b,c], [a,b,c], []).

n_huge_empty :-
  split_at(1000, [], [], []).

unbound_l :-
  split_at(2, L, [a,b], [c,d]),
  L == [a,b,c,d].

unbound_l_huge :-
  split_at(400, L, [a,b,c], []),
  L == [a,b,c].

unbound_l_take :-
  split_at(3, L, Take, [d]),
  L = [A,B,C,d],
  Take = [A,B,C],
  maplist(var, Take).

unbound_l_take_small :-
  split_at(3, L, Take, [d,e,f]),
  Take = [A,B,C],
  L = [A,B,C,d,e,f],
  maplist(var, Take).

unbound_l_take_empty :-
  forall(
    split_at(5, L, Take, []),
    (  length(L, N),
       N =< 5,
       term_variables(L, Vs),
       length(Vs, N),
       L == Take
    )
  ).

zero_unbound_l_take :-
  split_at(0, L, Take, [a,b]),
  L == [a,b],
  Take == [].
