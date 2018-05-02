:- module(list_util,
          [ cycle/2
          , drop/3
          , drop_while/3
          , group/2
          , group_by/3
          , group_with/3
          , iterate/3
          , keysort_r/2
          , lazy_findall/3
          , lazy_include/3
          , lazy_maplist/3
          , lines/2
          , map_include/3
          , map_include/4
          , map_include/5
          , maximum/2
          , maximum_by/3
          , maximum_with/3
          , minimum/2
          , minimum_by/3
          , minimum_with/3
          , msort_r/2
          , oneof/2
          , positive_integers/1
          , repeat/2
          , replicate/3
          , sort_by/3
          , sort_r/2
          , sort_with/3
          , span/4
          , span/5
          , split/3
          , split_at/4
          , take/3
          , take_while/3
          , xfy_list/3
          ]).
:- use_module(library(apply_macros)).  % for faster maplist/2
:- use_module(library(pairs), [group_pairs_by_key/2, map_list_to_pairs/3, pairs_values/2]).
:- use_module(library(readutil), [read_line_to_string/2]).
:- use_module(library(when), [when/2]).

:- include(nblist).
:- include(lazy_findall).
:- include(lines).

% TODO look through list library of Amzi! Prolog for ideas: http://www.amzi.com/manuals/amzi/libs/list.htm
% TODO look through ECLiPSe list library: http://www.eclipseclp.org/doc/bips/lib/lists/index.html

%% split(?Combined:list, ?Separator, ?Separated:list(list)) is det.
%
%  True if lists in Separated joined together with Separator form
%  Combined.  Can be used to split a list into sublists
%  or combine several sublists into a single list.
%
%  For example,
%  ==
%  ?- portray_text(true).
%
%  ?- split("one,two,three", 0',, Parts).
%  Parts = ["one", "two", "three"].
%
%  ?- split(Codes, 0',, ["alpha", "beta"]).
%  Codes = "alpha,beta".
%  ==
split([], _, [[]]) :-
    !.  % optimization
split([Div|T], Div, [[]|Rest]) :-
    split(T, Div, Rest),  % implies: dif(Rest, [])
    !.
split([H|T], Div, [[H|First]|Rest]) :-
    split(T, Div, [First|Rest]).


%% take(+N:nonneg, ?List:list, ?Front:list) is det.
%
%  True if Front contains the first N elements of List.
%  If N is larger than List's length, =|List=Front|=.
%
%  For example,
%  ==
%  ?- take(2, [1,2,3,4], L).
%  L = [1, 2].
%
%  ?- take(2, [1], L).
%  L = [1].
%
%  ?- take(2, L, [a,b]).
%  L = [a, b|_G1055].
%  ==
take(N, List, Front) :-
    split_at(N, List, Front, _).


%% split_at(+N:nonneg, ?Xs:list, ?Take:list, ?Rest:list)
%
%  True if Take is a list containing the first N elements of Xs and Rest
%  contains the remaining elements. If N is larger than the length of Xs,
%  =|Xs = Take|=.
%
%  For example,
%  ==
%  ?- split_at(3, [a,b,c,d], Take, Rest).
%  Take = [a, b, c],
%  Rest = [d].
%
%  ?- split_at(5, [a,b,c], Take, Rest).
%  Take = [a, b, c],
%  Rest = [].
%
%  ?- split_at(2, Xs, Take, [c,d]).
%  Xs = [_G3219, _G3225, c, d],
%  Take = [_G3219, _G3225].
%
%  ?- split_at(1, Xs, Take, []).
%  Xs = Take, Take = [] ;
%  Xs = Take, Take = [_G3810].
%  ==
split_at(N,Xs,Take,Rest) :-
    split_at_(Xs,N,Take,Rest).

split_at_(Rest, 0, [], Rest) :- !. % optimization
split_at_([], N, [], []) :-
    % cannot optimize here because (+, -, -, -) would be wrong,
    % which could possibly be a useful generator.
    N > 0.
split_at_([X|Xs], N, [X|Take], Rest) :-
    N > 0,
    succ(N0, N),
    split_at_(Xs, N0, Take, Rest).


%% take_while(:Goal, +List1:list, -List2:list) is det.
%
%  True if List2 is the longest prefix of List1 for which Goal succeeds.
%  For example,
%
%  ==
%  even(X) :- 0 is X mod 2.
%
%  ?- take_while(even, [2,4,6,9,12], Xs).
%  Xs = [2,4,6].
%  ==
:- meta_predicate take_while(1,+,-).
take_while(Goal, List, Prefix) :-
    span(Goal,List,Prefix,_).


% Define an empty_list type to assist with drop/3 documentation
:- multifile error:has_type/2.
error:has_type(empty_list, []).

%% drop(+N:nonneg, ?List:list, ?Rest:list) is det.
%% drop(+N:positive_integer, -List:list, +Rest:empty_list) is multi.
%
%
%  True if Rest is what remains of List after dropping the first N
%  elements.  If N is greater than List's length, =|Rest = []|=.
%
%  For example,
%  ==
%  ?- drop(1, [a,b,c], L).
%  L = [b, c].
%
%  ?- drop(10, [a,b,c], L).
%  L = [].
%
%  ?- drop(1, L, [2,3]).
%  L = [_G1054, 2, 3].
%
%  ?- drop(2, L, []).
%  L = [] ;
%  L = [_G1024] ;
%  L = [_G1024, _G1027].
%  ==
drop(N, List, Rest) :-
    % see Note_drop_as_split
    drop_(List, N, Rest).

drop_(L, 0, L) :-
    !.  % optimization
drop_([], N, []) :-
    N > 0.
drop_([_|T], N1, Rest) :-
    N1 > 0,
    succ(N0, N1),
    drop_(T, N0, Rest).

% Note_drop_as_split:
%
% drop/3 could be implemented as `split_at(N,List,_,Rest)`.  Unfortunately, that
% consumes memory building up a list of dropped elements only to throw it away.
% That would make something like drop(1_000_000,List,Rest) too expensive.


%% drop_while(:Goal, +List1:list, -List2:list) is det.
%
%  True if List2 is the suffix remaining after
%  =|take_while(Goal,List1,_)|=.  For example,
%
%  ==
%  even(X) :- 0 is X mod 2.
%
%  ?- drop_while(even, [2,4,6,9,12], Xs).
%  Xs = [9,12].
%  ==
:- meta_predicate drop_while(1,+,-).
drop_while(Goal, List, Suffix) :-
    span(Goal,List,_,Suffix).


%% span(:Goal, +List:list, -Prefix:list, -Suffix:list) is det.
%% span(:Goal, +List:list, +Prefix:list, -Suffix:list) is semidet.
%% span(:Goal, +List:list, -Prefix:list, +Suffix:list) is semidet.
%% span(:Goal, +List:list, +Prefix:list, +Suffix:list) is semidet.
%
%  True if Prefix is the longest prefix of List for which Goal
%  succeeds and Suffix is the rest. For any Goal, it is true that
%  =|append(Prefix,Suffix,List)|=. span/4 behaves as if it were
%  implement as follows (but it's more efficient):
%
%      span(Goal,List,Prefix,Suffix) :-
%          take_while(Goal,List,Prefix),
%          drop_while(Goal,List,Suffix).
%
%  For example,
%
%  ==
%  even(X) :- 0 is X mod 2.
%
%  ?- span(even, [2,4,6,9,12], Prefix, Suffix).
%  Prefix = [2,4,6],
%  Suffix = [9,12].
%  ==
:- meta_predicate span(1,+,-,-).
span(Goal, List, Prefix, Suffix) :-
    span_(List, Prefix, [], Suffix, Goal).


%% span(:Goal, +List:list, -Prefix:list, ?Tail:list, -Suffix:list) is semidet.
%
%  This is a version of span/4 that supports difference lists.
%
%  ==
%  ?- span(==(a), [a,a,b,c,a], Prefix, Tail, Suffix).
%  Prefix = [a, a|Tail],
%  Suffix = [b, c, a].
%  ==
:- meta_predicate span(1,+,-,?,-), span_(+,-,?,-,1).
span(Goal, List, Prefix, Tail, Suffix) :-
    span_(List, Prefix, Tail, Suffix, Goal).

span_([], Tail, Tail, [], _).
span_([H|Rest], Prefix, Tail, Suffix, Goal) :-
    ( call(Goal, H) ->
        Prefix = [H|Pre],
        span_(Rest, Pre, Tail, Suffix, Goal)
    ; % otherwise ->
        Suffix = [H|Rest],
        Tail = Prefix
    ).

%% replicate(?N:nonneg, ?X:T, ?Xs:list(T))
%
%  True only if Xs is a list containing only the value X repeated N times. If N is
%  less than zero, Xs is the empty list.
%
%  For example,
%  ==
%  ?- replicate(4, q, Xs).
%  Xs = [q, q, q, q] ;
%  false.
%
%  ?- replicate(N, X, [1,1]).
%  N = 2,
%  X = 1.
%
%  ?- replicate(0, ab, []).
%  true.
%
%  ?- replicate(N, X, Xs).
%  N = 0,
%  Xs = [] ;
%  N = 1,
%  Xs = [X] ;
%  N = 2,
%  Xs = [X, X] ;
%  N = 3,
%  Xs = [X, X, X] ;
%  ... etc.
%  ==
replicate(N,X,Xs) :-
    length(Xs,N),
    maplist(=(X),Xs).


%% repeat(?X, -Xs:list)
%
%  True if Xs is an infinite lazy list that only contains occurences of X. If X
%  is nonvar on entry, then all members of Xs will be constrained to be the same
%  term.
%
%  For example,
%  ==
%  ?- repeat(term(X), Rs), Rs = [term(2),term(2)|_].
%  X = 2
%  Rs = [term(2), term(2)|_G3041]
%
%  ?- repeat(X, Rs), take(4, Rs, Repeats).
%  Rs = [X, X, X, X|_G3725],
%  Repeats = [X, X, X, X]
%
%  ?- repeat(12, Rs), take(2, Rs, Repeats).
%  Rs = [12, 12|_G3630],
%  Repeats = [12, 12]
%  ==
repeat(X, Xs) :-
    cycle([X], Xs).


%% cycle(?Sequence, +Xs:list)
%
%  True if Xs is an infinite lazy list that contains Sequence, repeated cyclically.
%
%  For example,
%  ==
%  ?- cycle([a,2,z], Xs), take(5, Xs, Cycle).
%  Xs = [a, 2, z, a, 2|_G3765],
%  Cycle = [a, 2, z, a, 2]
%
%  ?- dif(X,Y), cycle([X,Y], Xs), take(3, Xs, Cycle), X = 1, Y = 12.
%  X = 1,
%  Y = 12,
%  Xs = [1, 12, 1|_G3992],
%  Cycle = [1, 12, 1]
%  ==
cycle(Sequence, Cycle) :-
    iterate(stack, Sequence-Sequence, Cycle).

% The state is best described as a stack that pops X and updates the state of the
% stack to Xs. If the state of the stack is empty, then the stack is reset to the
% full stack.
stack([]-[X|Xs], Xs-[X|Xs], X).
stack([X|Xs]-Stack, Xs-Stack, X).


%% oneof(List:list(T), Element:T) is semidet.
%
%  Same as memberchk/2 with argument order reversed. This form is
%  helpful when used as the first argument to predicates like include/3
%  and exclude/3.
oneof(Xs,X) :-
    memberchk(X, Xs).


%% map_include(:Goal:callable, +In:list, -Out:list) is det.
%
%  True if Out (elements =Yi=) contains those elements of In
%  (=Xi=) for which
%  =|call(Goal, Xi, Yi)|= is true.  If =|call(Goal, Xi, Yi)|= fails,
%  the corresponding element is omitted from Out.  If Goal generates
%  multiple solutions, only the first one is taken.
%
%  For example, assuming =|f(X,Y) :- number(X), succ(X,Y)|=
%  ==
%  ?- map_include(f, [1,a,3], L).
%  L = [2, 4].
%  ==
:- meta_predicate map_include(2, +, -).
:- meta_predicate map_include_(+, -, 2).
map_include(F, L0, L) :-
    map_include_(L0, L, F).

map_include_([], [], _).
map_include_([H0|T0], List, F) :-
    (   call(F, H0, H)
    ->  List = [H|T],
        map_include_(T0, T, F)
    ;   map_include_(T0, List, F)
    ).

%% map_include(:Goal:callable, +In0:list, +In1:list, -Out:list) is det.
%
%  Same as map_include/3, except Goal is binary argument meta predicate.
:- meta_predicate map_include(3, +, +, -).
:- meta_predicate map_include_(+, +, -, 3).
map_include(F, L0, L1, L) :-
    map_include_(L0, L1, L, F).

map_include_([], [], [], _).
map_include_([H0|T0], [H1|T1], List, F) :-
    (  call(F, H0, H1, H)
    -> List = [H|T],
       map_include_(T0, T1, T, F)
    ;  map_include_(T0, T1, List, F)
    ).

%% map_include(:Goal:callable, +In0:list, +In1:list, +In2:list, -Out:list) is det.
%
%  Same as map_include/3, except Goal is tertiary argument meta predicate.
:- meta_predicate map_include(4, +, +, +, -).
:- meta_predicate map_include_(+, +, +, -, 4).
map_include(F, L0, L1, L2, L) :-
    map_include_(L0, L1, L2, L, F).

map_include_([], [], [], [], _).
map_include_([H0|T0], [H1|T1], [H2|T2], List, F) :-
    (  call(F, H0, H1, H2, H)
    -> List = [H|T],
       map_include_(T0, T1, T2, T, F)
    ;  map_include_(T0, T1, T2, List, F)
    ).


%% maximum(?List:list, ?Maximum) is semidet.
%
%  True if Maximum is the largest element of List, according to
%  compare/3.  The same as `maximum_by(compare, List, Maximum)`.
maximum(List, Maximum) :-
    maximum_by(compare, List, Maximum).

%% maximum_with(:Goal, ?List:list, ?Maximum) is semidet.
%
%  True if Maximum is the largest projected value (according to compare/3) of
%  each element in the list. The projected values are found by applying `Goal`
%  to each list element.
:- meta_predicate maximum_with(2,?,?).
maximum_with(Project, List, Maximum) :-
    map_list_to_pairs(Project, List, Pairs),
    maximum_by(compare, Pairs, _-Maximum).

%% maximum_by(+Compare, ?List:list, ?Maximum) is semidet.
%
%  True if Maximum is the largest element of List, according to
%  Compare. Compare should be a predicate with the same signature as
%  compare/3.
%
%  If List is not ground the constraint is delayed until List becomes
%  ground.
:- meta_predicate maximum_by(3,?,?).
:- meta_predicate maximum_by(?,3,?,?).
maximum_by(Compare, List, Maximum) :-
    \+ ground(List),
    !,
    when(ground(List), maximum_by(Compare,List,Maximum)).
maximum_by(Compare,[H|T],Maximum) :-
    maximum_by(T, Compare, H, Maximum).
maximum_by([], _, Maximum, Maximum).
maximum_by([H|T], Compare, MaxSoFar, Maximum) :-
    call(Compare, Order, H, MaxSoFar),
    ( Order = (>) ->
        maximum_by(T, Compare, H, Maximum)
    ; % otherwise ->
        maximum_by(T, Compare, MaxSoFar, Maximum)
    ).


%% minimum(?List:list, ?Minimum) is semidet.
%
%  True if Minimum is the smallest element of List, according to
%  compare/3.  The same as `minimum_by(compare, List, Minimum)`.
minimum(List, Minimum) :-
    minimum_by(compare, List, Minimum).


%% minimum_with(:Goal, ?List:list, ?Minimum) is semidet.
%
%  True if Minimum is the largest projected value (according to compare/3) of
%  each element in the list. The projected values are found by applying `Goal`
%  to each list element.
:- meta_predicate minimum_with(2,?,?).
minimum_with(Project, List, Minimum) :-
    map_list_to_pairs(Project, List, Pairs),
    minimum_by(compare, Pairs, _-Minimum).

%% minimum_by(+Compare, ?List:list, ?Minimum) is semidet.
%
%  True if Minimum is the smallest element of List, according to
%  Compare. Compare should be a predicate with the same signature as
%  compare/3.
%
%  If List is not ground the constraint is delayed until List becomes
%  ground.
:- meta_predicate minimum_by(3,?,?).
:- meta_predicate minimum_by(?,3,?,?).
minimum_by(Compare, List, Minimum) :-
    \+ ground(List),
    !,
    when(ground(List), minimum_by(Compare,List,Minimum)).
minimum_by(Compare,[H|T],Minimum) :-
    minimum_by(T, Compare, H, Minimum).
minimum_by([], _, Minimum, Minimum).
minimum_by([H|T], Compare, MinSoFar, Minimum) :-
    call(Compare, Order, H, MinSoFar),
    ( Order = (<) ->
        minimum_by(T, Compare, H, Minimum)
    ; % otherwise ->
        minimum_by(T, Compare, MinSoFar, Minimum)
    ).


%% iterate(:Goal, +State, -List:list)
%
%  List is a lazy (possibly infinite) list whose elements are
%  the result of repeatedly applying Goal to State. Goal may fail to end
%  the list. Goal is called like
%
%      call(Goal, State0, State, Value)
%
%  The first value in List is the value produced by calling
%  Goal with State. For example, a lazy, infinite list of positive
%  integers might be defined with:
%
%      incr(A,B,A) :- succ(A,B).
%      integers(Z) :- iterate(incr,1,Z). % Z = [1,2,3,...]
%
%  Calling iterate/3 with a mode different than described in the
%  modeline throws an exception. Other modes may be supported in the
%  future, so don't rely on the exception to catch your mode errors.
:- meta_predicate iterate(3,+,?), iterate_(3,+,?).
iterate(Goal, State, List) :-
    must_be(nonvar, Goal),
    must_be(nonvar, State),
    freeze(List, iterate_(Goal, State, List)).

iterate_(Goal, State0, List) :-
    ( call(Goal, State0, State, X) ->
        List = [X|Xs],
        iterate(Goal, State, Xs)
    ; % goal failed, list is done ->
        List = []
    ).

%% positive_integers(-List:list(positive_integer)) is det.
%
%  Unifies List with a lazy, infinite list of all positive integers.
positive_integers(List) :-
    iterate(positive_integers_, 1, List).

positive_integers_(A,B,A) :-
    succ(A,B).


%% lazy_include(+Goal, +List1:list, -List2:list) is det.
%
%  Like include/3 but produces List2 lazily. This predicate is helpful
%  when List1 is infinite or very large.
:- meta_predicate lazy_include(1,+,-), lazy_include_(+,1,-).
lazy_include(Goal, Original, Lazy) :-
    freeze(Lazy, lazy_include_(Original, Goal, Lazy)).

lazy_include_([], _, []).
lazy_include_([H|T], Goal, Lazy) :-
    ( call(Goal, H) ->
        Lazy = [H|Rest],
        freeze(Rest, lazy_include_(T, Goal, Rest))
    ; % exclude this element ->
        lazy_include_(T, Goal, Lazy)
    ).


%% lazy_maplist(:Goal, ?List1:list, ?List2:list)
%
%  True if List2 is a list of elements that all satisfy Goal applied to each
%  element of List1. This is a lazy version of maplist/3.
:- meta_predicate lazy_maplist(2, ?, ?), lazy_maplist_(?,?,2).
lazy_maplist(Goal, Xs, Ys) :-
    freeze(Ys, freeze(Xs, lazy_maplist_(Xs, Ys, Goal))).

lazy_maplist_([], [], _).
lazy_maplist_([X|Xs], [Y|Ys], Goal) :-
    call(Goal, X, Y),
    lazy_maplist(Goal, Xs, Ys).


%% group_with(:Goal, +List:list, -Grouped:list(list)) is det.
%
%  Groups elements of List using Goal to project something out of each
%  element. Elements are first sorted based on the projected value (like
%  sort_with/3) and then placed into groups for which the projected
%  values unify. Goal is invoked as
%  =|call(Goal,Elem,Projection)|=.
%
%  For example,
%
%      ?- group_with(atom_length, [a,hi,bye,b], Groups).
%      Groups = [[a,b],[hi],[bye]]
:- meta_predicate group_with(2,+,-).
group_with(Goal,List,Groups) :-
    map_list_to_pairs(Goal, List, Pairs),
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, KeyedGroups),
    pairs_values(KeyedGroups, Groups).

%% group_by(:Goal, +List:list, -Groups:list(list)) is det.
%% group_by(:Goal, -List:list, +Groups:list(list)) is semidet.
%
%  Groups elements of List using a custom Goal predicate to test for equality.
%  If Goal is true, then two elements compare as equal.
%  Goal takes the form
%
%  =|call(Goal, X, Y)|=
%
%  Adjacent and equal elements of List will be grouped together if and only if
%  Goal is true
%
%  For example,
%  ==
%  ?- group_by(==, `Mississippi`, Gs),
%  maplist([Codes,String]>>string_codes(String,Codes), Gs, Groups).
%
%  Groups = ["M", "i", "ss", "i", "ss", "i", "pp", "i"].
%  ==
:- meta_predicate group_by(2, +, -), group_by_(+,2,-), group_by_(?,?,2,?,?).
group_by(Goal,List,Groups) :-
    ( var(List), var(Groups) ->
        instantiation_error(List)
    ; otherwise ->
        group_by_(List,Goal,Groups)
    ).

group_by_([],_,[]) :- !.
group_by_([X|Rest],Goal,[[X|Group]|Groups]) :-
    group_by_(Rest,X,Goal,Group,Groups).

group_by_([],_,_,[],[]) :- !.
group_by_([Y|Rest],X,Goal,[Y|Group],Groups) :-
    call(Goal,X,Y),
    !,
    group_by_(Rest,Y,Goal,Group,Groups).
group_by_([Y|Rest],_,Goal,[],[[Y|Group]|Groups]) :-
    group_by_(Rest,Y,Goal,Group,Groups).


%% group(+List:list, -Groups:list(list)) is semidet.
%
%  True if Groups is a compressed version of the elements in List. This predicate uses term equality
%  per `==/2` as the comparison goal for group_by/2. See the description of group_by/2.

group(List, Groups) :-
    group_by(==, List, Groups).

%% sort_by(:Goal, +List:list, -Sorted:list) is det.
%
%  See sort_with/3. This name was assigned to the wrong predicate in
%  earlier versions of this library.  It now throws an exception.
%  It will eventually be replaced with a different implementation.
:- meta_predicate sort_by(2,+,-).
sort_by(_,_,_) :-
    throw("Predicate sort_by/2 does not exist. Use sort_with/2 instead").

%% sort_with(:Goal, +List:list, -Sorted:list) is det.
%
%  Sort a List of elements using Goal to project
%  something out of each element. This is often more natural than
%  creating an auxiliary predicate for predsort/3. For example, to sort
%  a list of atoms by their length:
%
%      ?- sort_with(atom_length, [cat,hi,house], Atoms).
%      Atoms = [hi,cat,house].
%
%  Standard term comparison is used to compare the results of Goal.
%  Duplicates are _not_ removed. The sort is stable.
%
%  If Goal is expensive, sort_with/3 is more efficient than predsort/3
%  because Goal is called once per element, O(N), rather than
%  repeatedly per element, O(N log N).
:- meta_predicate sort_with(2,+,-).
sort_with(Goal, List, Sorted) :-
    map_list_to_pairs(Goal, List, Pairs),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, Sorted).

%% sort_r(+List:list, -ReverseSorted:list) is det.
%
%  Like sort/2 but produces a list sorted in reverse order.
sort_r --> sort, reverse.


%% msort_r(+List:list, -ReverseSorted:list) is det.
%
%  Like msort/2 but produces a list sorted in reverse order.
msort_r --> msort, reverse.


%% keysort_r(+List:list, -ReverseSorted:list) is det.
%
%  Like keysort/2 but produces a list sorted in reverse order.
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
