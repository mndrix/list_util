%% lines(+Source, -Lines:list(string)) is det.
%
%  Lines is a lazy list of lines from Source. Source can be
%  one of:
%
%    * file(Filename) - read lines from a file
%    * stream(Stream) - read lines from a stream
%
% After the last line has been read, all relevant streams are
% automatically closed.
%
% Each line in Lines does not contain the line terminator.
lines(file(File), Lines) :-
    open(File, read, Stream),
    lines(stream(Stream), Lines).
lines(stream(Stream), Lines) :-
    flow_to_llist(line_flow(Stream),Lines).


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


at_eof(line_flow(Stream)) :-
    at_end_of_stream(Stream).


next(line_flow(Stream),Line) :-
    read_line_to_string(Stream,Line).


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
nblist_head_tail(nblist(_,H,T), Head, Tail) :-
    % do we already know the head value?
    H=known(V),
    !,
    Head = V,
    Tail = T.
nblist_head_tail(NbList, Head, Tail) :-
    % don't know the head value, read it from the stream
    NbList = nblist(Flow, unknown, _),  % don't unify in head, because nb_setarg/3
    next(Flow,H),
    nb_setarg(2, NbList, known(H)),
    nb_setarg(3, NbList, nblist(Flow,unknown,unknown)),

    % now that side effects are done we can unify
    Head = H,
    nblist(_,_,Tail) = NbList.
