:- module(swi_pengines, [pengine_collect/2, pengine_collect/4, pengine_wait/1]).

:- use_module(library(pengines)).

%!  pengine_collect(-Results, +Options) is det.
%!  pengine_collect(?Template, +Goal, -Results, +Options) is det.
%
%   Collects Prolog engine results.  Repackages   the  collect predicate
%   used by the Prolog engine tests. There is only one minor difference.
%   The number of replies maps to replies/1  in Options. Succeeds if not
%   provided but unites with the  integer   number  of  replies from all
%   engines whenever passed to Options.   Options  partitions into three
%   sub-sets: next options, state options and ask options.
%
%   The implementation utilises  a  mutable   state  dictionary  to pass
%   event-loop arguments and accumulate results.   So quite useful. Note
%   also that the second Goal argument  is *not* module sensitive. There
%   consequently is no meta-predicate declaration for it.
%
%   The arity-2 form of pengine_collect  expects that the pengine_create
%   options have asked a query. Otherwise the collect waits indefinitely
%   for the engines to stop.
%
%   @arg Results are the result terms, a list of successful Goal results
%   accumulated by appending results from all the running engines.

pengine_collect(Results, Options) :- pengine_collect(-, -, Results, Options).

pengine_collect(Template, Goal, Results, Options) :-
    partition(next_option, Options, NextOptions, Options1),
    partition(state_option, Options1, StateOptions, AskOptions),
    dict_create(State, state,
                [ results([]),
                  replies(0),
                  options(_{ask:AskOptions, next:NextOptions})
                | StateOptions
                ]),
    pengine_event_loop(collect_handler(Template, Goal, State), []),
    Results = State.results,
    option(replies(State.replies), Options, _).

next_option(next(_)).

state_option(stop_after(_)).

collect_handler(Template, Goal, State, create(Id, _)) :-
    Goal \== (-),
    !,
    pengine_property(Id, self(Id)),
    pengine_ask(Id, Goal, [template(Template)|State.options.ask]).
collect_handler(_, _, State, success(Id, Results, More)) :-
    append(State.results, Results, Results1),
    b_set_dict(results, State, Results1),
    Replies1 is State.replies + 1,
    b_set_dict(replies, State, Replies1),
    (   StopAfter = State.get(stop_after),
        length(Results1, Collected),
        Collected >= StopAfter
    ->  pengine_destroy(Id)
    ;   More == true
    ->  pengine_next(Id, State.options.next)
    ;   true
    ).

%!  pengine_wait(Options) is semidet.
%
%   Waits for Prolog engines to die.  It   takes  time to die. If alive,
%   wait for the engines  by  sampling   the  current  engine  and child
%   engines periodically. Options allows  you   to  override the default
%   number of retries (10) and the default   number  of retry delays (10
%   milliseconds). Fails if times out while  waiting for engines to die;
%   failure means that engines remain alive (else something when wrong).
%
%   The implementation makes internal  assumptions   about  the pengines
%   module.  It  accesses   the   dynamic    and   volatile   predicates
%   current_pengine/6 and child/2. The latter is thread local.

pengine_wait(Options) :-
    option(retry(Retry), Options, 10),
    option(delay(Delay), Options, 0.01),
    (   true
    ;   between(1, Retry, _),
        sleep(Delay)
    ),
    \+ pengines:current_pengine(_, _, _, _, _, _),
    \+ pengines:child(_, _),
    !.
