flow_to_llist(Flow,List) :-
    NbList = nblist(Flow, unknown, unknown),
    freeze(List, flow_to_llist_(List, NbList)).

flow_to_llist_([], NbList) :-
    nblist_empty(NbList),
    !.
flow_to_llist_([H|T], NbList0) :-
    nblist_head_tail(NbList0, Head, NbList),
    H = Head,
    ( nblist_empty(NbList) -> % terminate list as soon as possible
        T = []
    ; true -> % more content available, fetch it on demand
        freeze(T, flow_to_llist_(T, NbList))
    ).


%% at_eof(+Flow) is semidet.
%
%  True if Flow can provide no more values.  eof = "end of flow"
:- discontiguous at_eof/1.


%% next(+Flow,-X) is semidet.
%
%  True if X is the next value from Flow.  Fails if Flow can
%  provide no more values.  next/2 should only fail if it's
%  impossible to determine whether more values are available until
%  trying to fetch them.  Otherwise, it's more efficienct for at_eof/1
%  to just succeed in the first place.
:- discontiguous next/2.


%% finalize_value(+X0,-X) is det.
%
%  True if a flow value X0 is finalized to X.  This allows a flow to produce
%  values that are different from the ones a user finally sees.
:- discontiguous finalize_value/3.


% We want lines/2 to work on all streams, even if they can't be
% repositioned. For example, network streams communicating with a
% line-based protocol. However, the lazy list on which users operate
% must support backtracking so that it behaves as they expect.
%
% To support both goals, we build a custom, non-backtrackable list in
% memory. Data read from a stream is permanently stored in this list.
% The user's list backtracks as it pleases and reconstructs itself from
% the underlying, non-bactrackable list.
%
% nb_setarg/3 is picky about how it operates. Mistakes lead to
% unexpected backtracking behavior. I've noted these deviations from
% standard practice with "because nb_setarg/3" in the comments below.
%
% Incidentally, on a large file (57,000 lines), this implementation is
% about twice as fast (2s vs 4s) as the old one because it doesn't spend
% time calling set_stream_position/2.

% :- type nblist ---> nblist(stream, atomic, nblist).
%
% The second, atomic argument can be `known(Val)`, `unknown` or `end_of_file`.
% We can't use variables because nb_setarg/3.

% is our nblist empty?
nblist_empty(nblist(_,end_of_file,_)) :-
    !.
nblist_empty(NbList) :-
    NbList = nblist(Flow, _, _),  % don't unify in head, because nb_setarg/3
    at_eof(Flow),
    nb_setarg(2,NbList,end_of_file).


% access the head and tail of our nblist
nblist_head_tail(nblist(Flow,H,T), Head, Tail) :-
    % do we already know the head value?
    H=known(V),
    !,
    finalize_value(Flow,V,Head),
    Tail = T.
nblist_head_tail(NbList, Head, Tail) :-
    % don't know the head value, read it from the stream
    NbList = nblist(Flow, unknown, _),  % don't unify in head, because nb_setarg/3
    next(Flow,H),
    nb_setarg(2, NbList, known(H)),
    nb_setarg(3, NbList, nblist(Flow,unknown,unknown)),

    % now that side effects are done we can unify
    finalize_value(Flow,H,Head),
    nblist(_,_,Tail) = NbList.
