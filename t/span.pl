:- use_module(library(list_util)).

no(_) :- fail.
yes(_) :- true.
even(X) :- 0 is X mod 2.

:- dynamic side_effect/1.

side_effect(_) :-
    asserta((side_effect(_) :- !, fail)).

:- use_module(library(tap)).

never :-
    span(no, [a,b,c], Prefix, Suffix),
    Prefix == [],
    Suffix == [a,b,c].

always :-
    span(yes, [john,sue,alice], Prefix, Suffix),
    Prefix == [john,sue,alice],
    Suffix == [].

evens :-
    span(even, [2,4,6,9,12], Prefix, Suffix),
    Prefix == [2,4,6],
    Suffix == [9,12].

diff_list :-
    span(==(a), [a,a,b,c,a], Prefix, Tail, Suffix),
    Prefix == [a,a|Tail],
    Suffix == [b,c,a],
    var(Tail).

diff_list_all_matching_prefix :-
    span(==(a), [a,a], Prefix, Tail, Suffix),
    Prefix == [a,a|Tail],
    var(Tail),
    Suffix == [].

diff_list_evens_with_nonvar_tail :-
    span(even, [2,4,6,9,12], Prefix, [x,y,z,z,x], Suffix),
    Prefix == [2,4,6,x,y,z,z,x],
    Suffix == [9,12].

diff_list_always :-
    span(yes, [john,sue,alice], Prefix, Tail, Suffix),
    Prefix == [john,sue,alice|Tail],
    var(Tail),
    Suffix == [].

% It is possible to have strange test cases where prefixes are equal to suffixes i.e. [a] == [a]
% given the presence of side effects. This test ensures robustness against impure predicates.
diff_list_side_effects :-
    span(side_effect, [a,a], Prefix, Tail, Suffix),
    Prefix == [a|Tail],
    var(Tail),
    Suffix == [a].

% If Goal never matches, then the difference list should be empty. An empty
% difference list is indicated by two identical variables (ie, there is no
% difference between the front and the tail of that list).
diff_list_never :-
    span(no, [a,a,b,c,a], Tail, Tail, Suffix),
    Suffix == [a,a,b,c,a].

% Same as above case except with ground Prefix input.
diff_list_never_ground_prefix :-
    span(no, [a,a,b,c,a], [], Tail, Suffix),
    Tail == [],
    Suffix == [a,a,b,c,a].

% Same as above case except with ground Tail input.
diff_list_never_ground_tail :-
    span(no, [a,a,b,c,a], Prefix, [], Suffix),
    Prefix == [],
    Suffix == [a,a,b,c,a].

diff_list_never_ground_prefix_and_tail :-
    span(no, [a,a,b,c,a], [], [], Suffix),
    Suffix == [a,a,b,c,a].

% A common use case for span/5 is to match sequences of patterns. For example,
% this test does something similar to matching the regular expression "a*z*b*".
%
% Of course, a DCG would be more natural in this particular case.
chained :-
    span(==(a),[a,a,b,c,a],Match,Tail0,Rest0),
    span(==(z),Rest0,Tail0,Tail1,Rest1),
    span(==(b),Rest1,Tail1,[],Rest2),
    Match == [a,a,b],
    Rest2 == [c,a].
