:- module(list_util,
    [ split/3
    ]).
/** <module> Utilities for lists

A collection of predicates for working with lists.

@author Michael Hendricks <michael@ndrix.org>
@license BSD
*/

%%  split(?Combined:list, ?Separator, ?Separated:list(list)) is det.
%
%	True if lists in Separated joined together with Separator form
%	Combined.  Can be used to split a list into sublists
%	or combine several sublists into a single list.
%
%	For example,
%
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
