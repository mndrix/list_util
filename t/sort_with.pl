:- use_module(library(list_util)).
:- use_module(library(apply), [maplist/3]).
:- use_module(library(record)).

:- record person(name:string, age:positive_integer).

people([ person("John", 27)
       , person("Tom", 30)
       , person("Sue", 18)
       , person("Will", 63)
       ]).


:- use_module(library(tap)).

by_name :-
    people(People),
    sort_with(person_name, People, Sorted),
    maplist(person_name, Sorted, Names),
    Names == ["John","Sue","Tom","Will"].

by_age :-
    people(People),
    sort_with(person_age, People, Sorted),
    maplist(person_age, Sorted, Ages),
    Ages == [18, 27, 30, 63].
