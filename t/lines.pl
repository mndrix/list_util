:- use_module(library(list_util)).

sample(Lines) :-
    lines(file("t/lines.txt"), Lines).

:- use_module(library(tap)).


length :-
    sample(Lines),
    length(Lines, Len),  % built in length leaves choicepoints for lazy lists
    Len == 4.

content :-
    sample(Lines),
    Lines = [A,B,C,D],
    A == "alpha",
    B == "beta",
    C == "gamma",
    D == "delta".

partial :-
    sample(Lines),
    Lines = [Head|Rest],
    Head == "alpha",
    var(Rest).
