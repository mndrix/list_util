:- module(list_util,
          [ drop/3
          , keysort_r/2
          , map_include/3
          , msort_r/2
          , sort_r/2
          , split/3
          , take/3
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


:- begin_tests(split).
test(forward_zero) :-
    split("", 10, [""]).
test(forward_one) :-
    split("hello", 10, ["hello"]).
test(forward_two) :-
    split("hello\naravis", 10, ["hello", "aravis"]).
test(forward_three) :-
    split("hello\naravis\njericho", 10, ["hello","aravis","jericho"]).

test(backward_zero) :-
    split(Codes, 10, [[]]),
    Codes = [].
test(backward_one) :-
    split(Codes, 10, ["hello"]),
    Codes = "hello".
test(backward_two) :-
    [Comma] = ",",
    split(Codes, Comma, ["alpha", "beta"]),
    Codes = "alpha,beta".
test(backward_three) :-
    [Comma] = ",",
    split(Codes, Comma, ["alpha","beta","gamma"]),
    Codes = "alpha,beta,gamma".

test(find_separator) :-
    split("alpha,beta", Div, ["alpha","beta"]),
    [Div] = ",".

test(forward_trailing_zero) :-
    split("\n", 10, ["",""]).
test(forward_trailing_one) :-
    split("hello\n", 10, ["hello",""]).
test(forward_trailing_two) :-
    split("hello\ngoodbye\n", 10, ["hello", "goodbye", ""]).
:- end_tests(split).

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

:- begin_tests(take).
test(n_huge) :-
    take([a,b], 100, [a,b]).
test(n_huge_empty) :-
    take([], 100, []).
test(n_small) :-
    take([a,b,c,d], 2, [a,b]).
test(none) :-
    take([a,b,c], 0, []).
test(none_empty) :-
    take([], 0, []).

test(backward) :-
    take(L, 3, [a,b,c]),
    L = [a,b,c|T],
    var(T).
test(backward_huge) :-
    take(L, 300, [a,b]),
    L = [a,b].
:- end_tests(take).

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

:- begin_tests(drop).
test(zero) :-
    drop([a,b], 0, [a,b]).
test(one) :-
    drop([a,b], 1, [b]).
test(huge) :-
    drop([1,2,3], 99, []).

test(backward) :-
    drop(L, 3, [4,5]),
    L = [A, B, C, 4, 5],
    var(A), var(B), var(C).
test(backward_zero) :-
    drop(L, 0, [1,2,3]),
    L = [1,2,3].
test(backward_huge) :-
    forall(
        drop(L,3,[]),
        ( length(L,N)
        , N =< 3
        , term_variables(L, Vs)
        , length(Vs, N)
        )
    ).
:- end_tests(drop).


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


% predicates for use during testing
map_include_test_num(X,X) :- number(X).
map_include_test_f(X,Y) :- number(X), succ(X, Y).

:- begin_tests(map_include).
test(just_mapping) :-
    map_include(succ, [1,2,3], [2,3,4]).
test(just_filtering) :-
    map_include(map_include_test_num, [1,a,3], [1,3]).
test(both_mapping_and_filtering) :-
    map_include(map_include_test_f, [a,1,2,b], [2,3]).
:- end_tests(map_include).


%%	sort_r(+List:list, -ReverseSorted:list) is det.
%
%	Like sort/2 but produces a list sorted in reverse order.
sort_r --> sort, reverse.

:- begin_tests(sort_r).
test(atoms) :-
    sort_r([a,c,j,b], [j,c,b,a]).
test(duplicates) :-
    sort_r([3,m,4,1,m,9], [m,9,4,3,1]).
:- end_tests(sort_r).


%%	msort_r(+List:list, -ReverseSorted:list) is det.
%
%	Like msort/2 but produces a list sorted in reverse order.
msort_r --> msort, reverse.

:- begin_tests(msort_r).
test(atoms) :-
    msort_r([a,c,j,b], [j,c,b,a]).
test(duplicates) :-
    msort_r([3,m,4,1,m,9], [m,m,9,4,3,1]).
:- end_tests(msort_r).


%%	keysort_r(+List:list, -ReverseSorted:list) is det.
%
%	Like keysort/2 but produces a list sorted in reverse order.
keysort_r --> keysort, reverse.

:- begin_tests(keysort_r).
test(atom_int) :-
    keysort_r([a-1,c-3,b-2],[c-3,b-2,a-1]).
test(mixed) :-
    keysort_r([beta-2,9-nine,7.4-float], [beta-2,9-nine,7.4-float]).
:- end_tests(keysort_r).
