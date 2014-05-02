:- module(list_util,
          [ drop/3
          , keysort_r/2
          , lines/2
          , map_include/3
          , maximum/2
          , maximum_by/3
          , minimum/2
          , minimum_by/3
          , msort_r/2
          , iterate/3
          , sort_r/2
          , split/3
          , take/3
          , xfy_list/3
          ]).
:- use_module(library(readutil), [read_line_to_string/2]).
:- use_module(library(when), [when/2]).

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
:- multifile error:has_type/2.
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


%%	maximum(?List, ?Maximum) is semidet.
%
%   True if Maximum is the largest element of List, according to
%   compare/3.  The same as `maximum_by(compare, List, Maximum)`.
maximum(List, Maximum) :-
    maximum_by(compare, List, Maximum).


%%	maximum_by(+Compare, ?List, ?Maximum) is semidet.
%
%   True if Maximum is the largest element of List, according to
%   Compare. Compare should be a predicate with the same signature as
%   compare/3.
%
%   If List is not ground the constraint is delayed until List becomes
%   ground.
:- meta_predicate maximum_by(3,?,?).
:- meta_predicate maximum_by(?,3,?,?).
maximum_by(Compare, List, Minimum) :-
    \+ ground(List),
    !,
    when(ground(List), maximum_by(Compare,List,Minimum)).
maximum_by(Compare,[H|T],Minimum) :-
    maximum_by(T, Compare, H, Minimum).
maximum_by([], _, Minimum, Minimum).
maximum_by([H|T], Compare, MinSoFar, Minimum) :-
    call(Compare, Order, H, MinSoFar),
    ( Order = (>) ->
        maximum_by(T, Compare, H, Minimum)
    ; % otherwise ->
        maximum_by(T, Compare, MinSoFar, Minimum)
    ).


%%	minimum(?List, ?Minimum) is semidet.
%
%   True if Minimum is the smallest element of List, according to
%   compare/3.  The same as `minimum_by(compare, List, Minimum)`.
minimum(List, Minimum) :-
    minimum_by(compare, List, Minimum).


%%	minimum_by(+Compare, ?List, ?Minimum) is semidet.
%
%   True if Minimum is the smallest element of List, according to
%   Compare. Compare should be a predicate with the same signature as
%   compare/3.
%
%   If List is not ground the constraint is delayed until List becomes
%   ground.
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


%% iterate(:Goal, +State, -List)
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


%% lines(+Source, -Lines:list(string)) is det.
%
%  Lines is a lazy list of lines from Source. Source can be
%  one of:
%
%    * file(Filename) - read lines from a file
%    * stream(Stream) - read lines from a stream
%
% After the last line has been read, all relevant streams are
% automatically closed. One must be able to call set_stream_position/2
% on Stream.
%
% Each line in Lines does not contain the line terminator.
lines(file(Filename), Lines) :-
    open(Filename, read, Stream, []),
    lines(stream(Stream), Lines).
lines(stream(Stream), Lines) :-
    stream_property(Stream, position(Pos)),
    iterate(lines_, Stream-Pos, Lines).

lines_(Stream-Pos0, Stream-Pos, Line) :-
    is_stream(Stream), % don't touch a closed stream
    set_stream_position(Stream, Pos0), % to handle backtracking
    read_line_to_string(Stream, Tentative),
    ( Tentative = end_of_file ->
        close(Stream),
        fail  % to end the list of lines
    ; % there was a line ->
        stream_property(Stream, position(Pos)),
        Line = Tentative
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
