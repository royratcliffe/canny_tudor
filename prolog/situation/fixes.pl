:- module(situation_fixes,
          [   situation_now/2,
              situation_now/3,
              situation_fix/1,
              situation_fix/2
          ]).

:- predicate_options(situation_fix/2, 2,
                     [   now(any),
                         at(number),
                         delay(number)
                     ]).

:- meta_predicate
    situation_now(:, +),
    situation_now(:, +, +),
    situation_fix(:),
    situation_fix(:, +).

:- use_module(options).
:- use_module(properties).

%!  situation_now(?Situation:compound, +Now:any) is nondet.
%!  situation_now(?Situation:compound, +Now:any, +At:number) is nondet.
%
%   Makes some Situation become Now  for  time   index  At,  at the next
%   fixation. Effectively schedules a pending update  one or more times;
%   the next situation_fix/1 fixes the pending situation changes at some
%   future point. If no At time, arity two, uses the current time.
%
%   Uses  situation_options/2  when  Situation  is    ground,  but  uses
%   situation_property/2  otherwise.  Asserts  therefore   for  multiple
%   situations if Situation comprises variables.

situation_now(Situation, Now) :-
    get_time(At),
    situation_now(Situation, Now, At).

situation_now(Situation, Now, At) :-
    ground(Now),
    number(At),
    now(Situation, Now, At).

now(Situation, Now, At) :-
    ground(Situation),
    !,
    situation_options(Situation, [module(M)]),
    assertz(M:now(Now, At)).
now(Situation, Now, At) :-
    forall(situation_property(Situation, module(M)),
           assertz(M:now(Now, At))).

%!  situation_fix(?Situation:compound) is det.
%
%   Fixating situations does three important things.  First, it adds new
%   Previous-When pairs to the  situation   history.  They  become was/2
%   dynamic facts (clauses without rules). Second,  it adds, replaces or
%   removes the most current Current-When pair. This allows detection of
%   non-events, e.g. when something disappears.  Some types of situation
%   might  require  such  event  edges.   Finally,  fixating  broadcasts
%   situation-change messages.
%
%   The rule for fixing the Current-When pair goes like this: Is there a
%   new now/2, at least one? The  latest   becomes  the new current. Any
%   others become Previous-When. If there is  no now/2, then the current
%   disappears. Messages broadcast accordingly. If   there  is more than
%   one   now/2,   only   the    latest     becomes    current.    Hence
%   currently-previously only transitions once in-between fixations.

situation_fix(Situation) :-
    forall(situation_property(Situation, module(M)), fix(Situation, M)).

fix(Situation, M) :-
    findall(Now0-At0, retract(M:now(Now0, At0)), Fixes0),
    sort(2, @=<, Fixes0, Fixes),
    forall(member(Now-At, Fixes), asserta(M:was(Now, At))),
    ignore(retract(M:previously(_, _))),
    fix(Situation, M, Fixes),
    broadcast(situation:fixed(Situation)).

fix(Situation, M, []) :-
    once(retract(M:currently(Previous, At))),
    !,
    asserta(M:previously(Previous, At)),
    broadcast(situation:was(Situation, Previous, At)).
fix(_, _, []) :-
    !.
fix(Situation, M, Fixes) :-
    last(Fixes, Now-At),
    fix(Situation, M, Now, At).

fix(_, M, Now, _) :-
    once(M:currently(Now, _)),
    !.
fix(Situation, M, Now, At) :-
    once(retract(M:currently(Previous, When))),
    !,
    asserta(M:previously(Previous, When)),
    asserta(M:currently(Now, At)),
    broadcast(situation:was(Situation, Previous, When)),
    broadcast(situation:changed(Situation, Previous-When, Now-At)),
    broadcast(situation:now(Situation, Now, At)).
fix(Situation, M, Now, At) :-
    asserta(M:currently(Now, At)),
    broadcast(situation:now(Situation, Now, At)).

%!  situation_fix(?Situation:compound, +Options:list) is det.

situation_fix(Situation, Options) :-
    (   option(at(At), Options),
        number(At)
    ->  true
    ;   get_time(At)
    ),
    (   option(now(Now), Options)
    ->  situation_now(Situation, Now, At)
    ;   true
    ),
    situation_fix(Situation),
    (   option(delay(Delay), Options),
        number(Delay),
        situation_property(Situation, module(M))
    ->  When0 is At - Delay,
        forall((   M:was(Was, When),
                   When < When0
               ), once(retract(M:was(Was, When))))
    ;   true
    ).
