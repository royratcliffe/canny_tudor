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

:- use_module(options).
:- use_module(properties).

:- meta_predicate
    situation_now(:, +),
    situation_now(:, +, +),
    situation_fix(:),
    situation_fix(:, +).

%!  situation_now(?Situation:compound, +Now:any) is nondet.
%!  situation_now(?Situation:compound, +Now:any, +At:number) is nondet.

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

situation_fix(Situation) :-
    forall(situation_property(Situation, module(M)), fix(M)).

fix(M) :-
    findall(Now0-At0, retract(M:now(Now0, At0)), Fixes0),
    sort(2, @=<, Fixes0, Fixes),
    forall(member(Now-At, Fixes), asserta(M:was(Now, At))),
    ignore(retract(M:previously(_, _))),
    fix(M, Fixes).

fix(M, []) :-
    once(retract(M:currently(Previous, At))),
    !,
    asserta(M:previously(Previous, At)),
    broadcast(situation:was(Previous, At)).
fix(_, []) :-
    !.
fix(M, Fixes) :-
    last(Fixes, Now-At),
    fix(M, Now, At).

fix(M, Now, At) :-
    once(retract(M:currently(Previous, When))),
    !,
    asserta(M:previously(Previous, When)),
    asserta(M:currently(Now, At)),
    broadcast(situation:was(Previous, When)),
    broadcast(situation:changed(Previous-When, Now-At)),
    broadcast(situation:now(Now, At)).
fix(M, Now, At) :-
    asserta(M:currently(Now, At)),
    broadcast(situation:now(Now, At)).

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
