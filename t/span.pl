:- use_module(library(list_util)).

no(_) :- fail.
yes(_) :- true.
even(X) :- 0 is X mod 2.

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

diff_list_evens_with_nonvar_prefix :-
    span(even, [2,4,6,9,12], Prefix, [x,y,z,z,x], Suffix),
    Prefix == [2,4,6,x,y,z,z,x],
    Suffix == [9,12].

% If Prefix is an empty list then Tail shouldn't exist, therefore it is logical to fail here.
diff_list_never(fail) :-
    span(no, [a,a,b,c,a], _Prefix, _Tail, _Suffix).

% Same as above case except with ground Prefix input.
diff_list_never_ground_prefix(fail) :-
    span(no, [a,a,b,c,a], [], _Tail, _Suffix).

% Same as above case except with ground Tail input.
diff_list_never_ground_tail(fail) :-
    span(no, [a,a,b,c,a], _Prefix, [], _Suffix).

diff_list_never_tail_ground_prefix_and_tail(fail) :-
    span(no, [a,a,b,c,a], [], [], _Suffix).
