:- use_module(library(list_util)).
:- use_module(library(tap)).


none :-
  split_at(0, [a,b], [], [a,b]).

none_empty :-
  split_at(0, [], [], []).

n_small :-
  split_at(3, [a,b,c,d,e,f], [a,b,c], [d,e,f]).

n_huge :-
  aggregate_all(count, split_at(1000, [a,b,c], [a,b,c], []), 1).

n_huge_empty :-
  aggregate_all(count, split_at(1000, [], [], []), 1).

unbound_l :-
  split_at(2, L, [a,b], [c,d]),
  L = [a,b,c,d].

unbound_l_huge :-
  aggregate_all(count,
    (  split_at(400, L, [a,b,c], []),
       L = [a,b,c]
    ), 1).

unbound_l_take :-
  split_at(3, L, Take, [d]),
  forall(member(T, Take), var(T)),
  L = [A,B,C,d],
  maplist(var, [A,B,C]),
  maplist(var, Take).

unbound_l_take_huge :-
  split_at(3, L, Take, [c,d,e]),
  append(Take, [c,d,e], L),
  forall(member(T, Take), var(T)).

unbound_l_take_empty :-
  forall(split_at(5, L, Take, []),
    (  length(L, N),
       N =< 5,
       term_variables(L, Vs),
       length(Vs, N)
    )
  ),
  L = Take.

	
