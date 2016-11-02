%% lazy_findall(:Template,+Goal,-List:list) is det.
%
%  Like findall/3 but List is constructed lazily.  This allows it to be used
%  when Goal produces many (or infinite) solutions.
%
%  Goal is always executed at least once, even if it's not strictly necessary.
%  Goal may be executed in advance, even if the associated value in List has not
%  been demanded yet.  This should only be important if Goal performs side
%  effects whose timing is important to you.
%
%  If you don't consume all of List, it's likely that a worker thread will be
%  left hanging.  This is a temporary implementation detail which we hope to
%  resolve.
:- meta_predicate lazy_findall(?,0,?), lazy_findall_search(+,?,0).
lazy_findall(Template,Goal,List) :-
    lazy_findall_flow(Template,Goal,Flow),
    ( next(Flow,Value0) -> % eager first value (to detect instant failure)
        finalize_value(Flow,Value0,Value),
        List=[Value|Rest],
        flow_to_llist(Flow,Rest)
    ; otherwise ->
        List=[]
    ).

lazy_findall_search(ResponseQ,Template,Goal) :-
    call_ended(Goal,Ended),
    ( Ended=more -> % Goal has unexplored choicepoints
        thread_get_message(send_a_solution),
        thread_send_message(ResponseQ,a_solution(Template)),
        fail % explore other choicepoints
    ; Ended=done -> % Goal found final solution
        lazy_findall_final(ResponseQ,just(Template))
    ; Ended=fail ->
        lazy_findall_final(ResponseQ,nothing)
    ; Ended=exception(E) ->
        lazy_findall_final(ResponseQ,exception(E))
    ).

lazy_findall_final(ResponseQ,Solution) :-
    thread_send_message(ResponseQ,final_solution),
    thread_exit(Solution).


%% call_ended(:Goal,-Status) is det.
%
%  True if calling Goal ends as indicated by Status.  Status is one of
%  the following:
%
%    * `fail` - Goal failed
%    * `done` - Goal produced its final solution (no choicepoints left)
%    * `more` - backtracking will produce more solutions
%    * `exception(E)` - Goal threw an exception
:- meta_predicate call_ended(0,?).
call_ended(Goal,End) :-
    ( catch(call_cleanup(Goal,End=done),E,End=exception(E)) *->
        ( var(End) -> End=more ; ! )
    ; otherwise ->
        End=fail, !  % Goal failed immediately
    ).
call_ended(_,fail).  % Goal fails on backtracking


:- meta_predicate lazy_findall_flow(?,0,?).
lazy_findall_flow(Template,Goal,Flow) :-
    gensym(lazy_findall_,ThreadAlias),  % see Note_findall_alias
    message_queue_create(ResponseQ,[max_size(1)]),
    thread_create(
        lazy_findall_search(ResponseQ,Template,Goal),
        _ThreadId,
        [alias(ThreadAlias)]
    ),
    Flow = findall_flow(ThreadAlias,ResponseQ).

/*
Note_findall_alias:

To implement flow:at_eof/1 we must be able to recognize when a thread no longer
exists. SWI-Prolog's default thread IDs are reused after a thread's resources
are garbage collected. If we used the default thread IDs, we might encounter the
following scenario:

  1. we start lazy_findall/3
  2. our worker thread exits
  3. another thread starts
  4. at_eof/1 thinks the worker thread still exists

By using gensym/2 and a thread alias, we can be certain that our thread ID is
never reused.
*/

at_eof(findall_flow(Thread,_ResponseQ)) :-
    ( thread_exists(Thread) ->
        thread_property(Thread,status(exited(nothing))),
        thread_join(Thread,_)
    ; otherwise -> % worker is gone. flow is finished
        true
    ),
    % invariant at this point: worker thread is gone
    true.

thread_exists(Thread) :-
    catch(thread_property(Thread,status(_)), _, fail).


next(findall_flow(Thread,ResponseQ),Solution) :-
    catch(thread_send_message(Thread,send_a_solution),_,true), % maybe thread's gone
    thread_get_message(ResponseQ,Response),
    findall_next(Response,Thread,Solution).

findall_next(a_solution(X),_,just(X)).
findall_next(final_solution,Thread,Solution) :-
    % final solution is waiting. join worker thread to receive it
    thread_join(Thread,exited(MaybeSolution)),
    ( MaybeSolution=nothing ->
        fail  % final solution was a spurious choicepoint
    ; otherwise ->
        Solution=MaybeSolution
    ).


finalize_value(findall_flow(_,_),X0,X) :-
    lazy_findall_finalize_value(X0,X).

lazy_findall_finalize_value(just(X),X).
lazy_findall_finalize_value(exception(E),_) :-
    throw(E).
