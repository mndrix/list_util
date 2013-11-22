:- use_module(library(list_util)).
:- use_module(library(tap)).

:- set_prolog_flag(double_quotes, codes).

forward_zero :-
    split("", 10, [""]).
forward_one :-
    split("hello", 10, ["hello"]).
forward_two :-
    split("hello\naravis", 10, ["hello", "aravis"]).
forward_three :-
    split("hello\naravis\njericho", 10, ["hello","aravis","jericho"]).

backward_zero :-
    split(Codes, 10, [[]]),
    Codes = [].
backward_one :-
    split(Codes, 10, ["hello"]),
    Codes = "hello".
backward_two :-
    [Comma] = ",",
    split(Codes, Comma, ["alpha", "beta"]),
    Codes = "alpha,beta".
backward_three :-
    [Comma] = ",",
    split(Codes, Comma, ["alpha","beta","gamma"]),
    Codes = "alpha,beta,gamma".

find_separator :-
    split("alpha,beta", Div, ["alpha","beta"]),
    [Div] = ",".

forward_trailing_zero :-
    split("\n", 10, ["",""]).
forward_trailing_one :-
    split("hello\n", 10, ["hello",""]).
forward_trailing_two :-
    split("hello\ngoodbye\n", 10, ["hello", "goodbye", ""]).
