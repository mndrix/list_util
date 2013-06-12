:- module(list_util,
          [ drop/3
          , keysort_r/2
          , map_include/3
          , msort_r/2
          , sort_r/2
          , split/3
          , take/3
          , xfy_list/3
          ]).
%%  split(?Combined:list, ?Separator, ?Separated:list(list)) is det.
%
%	True if lists in Separated joined together with Separator form
%	Combined.  Can be used to split a list into sublists
%	or combine several sublists into a single list.
%
%	For example,
%	==
%	?- portray_text(true).
%
%	?- split("one,two,three", 0',, Parts).
%	Parts = ["one", "two", "three"].
%
%	?- split(Codes, 0',, ["alpha", "beta"]).
%	Codes = "alpha,beta".
%	==
split([], _, [[]]) :-
    !.  % optimization
split([Div|T], Div, [[]|Rest]) :-
    split(T, Div, Rest),  % implies: dif(Rest, [])
    !.  % optimization
split([H|T], Div, [[H|First]|Rest]) :-
    dif(H, Div),
    split(T, Div, [First|Rest]).


%%	take(?List:list, +N:nonneg, ?Front:list) is det.
%
%	True if Front contains the first N elements of List.
%	If N is larger than List's length, =|List=Front|=.
%
%	For example,
%	==
%	?- take([1,2,3,4], 2, L).
%	L = [1, 2].
%
%	?- take([1], 2, L).
%	L = [1].
%
%	?- take(L, 2, [a,b]).
%	L = [a, b|_G1055].
%	==
take([], N, []) :-
    N > 0,
    !.  % optimization
take(_, 0, []) :-
    !.  % optimization
take([H|T], N1, [H|Rest]) :-
    N1 > 0,
    succ(N0, N1),
    take(T, N0, Rest).


% Define an empty_list type to assist with drop/3 documentation
error:has_type(empty_list, []).

%%	drop(?List:list, +N:nonneg, ?Rest:list) is det.
%%	drop(-List:list, +N:positive_integer, +Rest:empty_list) is multi.
%
%
%	True if Rest is what remains of List after dropping the first N
%	elements.  If N is greater than List's length, =|Rest = []|=.
%
%	For example,
%	==
%	?- drop([a,b,c], 1, L).
%	L = [b, c].
%
%	?- drop([a,b,c], 10, L).
%	L = [].
%
%	?- drop(L, 1, [2,3]).
%	L = [_G1054, 2, 3].
%
%	?- drop(L, 2, []).
%	L = [] ;
%	L = [_G1024] ;
%	L = [_G1024, _G1027].
%	==
drop(L, 0, L) :-
    !.  % optimization
drop([], N, []) :-
    N > 0.
drop([_|T], N1, Rest) :-
    N1 > 0,
    succ(N0, N1),
    drop(T, N0, Rest).



%%	map_include(:Goal:callable, +In:list, -Out:list) is det.
%
%	True if Out (elements =Yi=) contains those elements of In
%	(=Xi=) for which
%	=|call(Goal, Xi, Yi)|= is true.  If =|call(Goal, Xi, Yi)|= fails,
%	the corresponding element is omitted from Out.  If Goal generates
%	multiple solutions, only the first one is taken.
%
%	For example, assuming =|f(X,Y) :- number(X), succ(X,Y)|=
%	==
%	?- map_include(f, [1,a,3], L).
%	L = [2, 4].
%	==
:- meta_predicate map_include(2, +, -).
map_include(F, L0, L) :-
    map_include_(L0, F, [], ReversedL),
    reverse(ReversedL, L).
map_include_([], _, Accum, Accum).
map_include_([H0|T], F, Accum0, List) :-
    (   call(F, H0, H)
    ->  map_include_(T, F, [H|Accum0], List)
    ;   map_include_(T, F,    Accum0 , List)
    ).



%%	sort_r(+List:list, -ReverseSorted:list) is det.
%
%	Like sort/2 but produces a list sorted in reverse order.
sort_r --> sort, reverse.


%%	msort_r(+List:list, -ReverseSorted:list) is det.
%
%	Like msort/2 but produces a list sorted in reverse order.
msort_r --> msort, reverse.


%%	keysort_r(+List:list, -ReverseSorted:list) is det.
%
%	Like keysort/2 but produces a list sorted in reverse order.
keysort_r --> keysort, reverse.


%%  xfy_list(?Op:atom, ?Term, ?List) is det.
%
%   True if elements of List joined together with xfy operator Op gives
%   Term. Usable in all directions.
%
%   For example,
%
%   ==
%   ?- xfy_list(',', (a,b,c), L).
%   L = [a, b, c].
%
%   ?- xfy_list(Op, 4^3^2, [4,3,2]).
%   Op = (^).
%   ==
xfy_list(Op, Term, [Left|List]) :-
    Term =.. [Op, Left, Right],
    xfy_list(Op, Right, List),
    !.
xfy_list(_, Term, [Term]).
