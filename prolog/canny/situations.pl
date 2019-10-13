:- module(canny_situations, [situation/2, situation_property/2]).

:- meta_predicate
    situation(:, ?),
    situation_property(:, ?).

:- multifile
    canny:situation/2,
    canny:situation_property/2.

:- use_module(library(random/temporary)).

:- dynamic situation_module/2.

situation(Situation, Term) :-
    canny:situation(Situation, Term).

canny:situation(Situation, module(Module)) :-
    with_mutex(canny_situations, temporary_module(Situation, Module)).

temporary_module(Situation, Module) :-
    situation_module(Situation, Module),
    !.
temporary_module(Situation, Module) :-
    ground(Situation),
    once(random_temporary_module(Module)),
    asserta(situation_module(Situation, Module)),
    dynamic([   Module:now/2,
                Module:was/2,
                Module:currently/2,
                Module:previously/2
            ], []).

canny:situation(Situation, now(Now, At)) :-
    ground(Now),
    number(At),
    now(Situation, Now, At).

now(Situation, Now, At) :-
    ground(Situation),
    !,
    canny:situation(Situation, module(Module)),
    assertz(Module:now(Now, At)).
now(Situation, Now, At) :-
    forall(canny:situation(Situation, module(Module)),
           assertz(Module:now(Now, At))).

canny:situation(Situation, fix) :-
    situation_module(Situation, Module),
    fix(Situation, Module).

fix(Situation, Module) :-
    findall(Now0-At0, retract(Module:now(Now0, At0)), Fixes0),
    sort(2, @=<, Fixes0, Fixes),
    forall(member(Now-At, Fixes), asserta(Module:was(Now, At))),
    ignore(retract(Module:previously(_, _))),
    fix(Fixes, Situation, Module),
    broadcast(situation(Situation, fix)).

fix([], Situation, Module) :-
    once(retract(Module:currently(Previous, When))),
    !,
    asserta(Module:previously(Previous, When)),
    broadcast(situation:was(Situation, Previous, When)).
fix([], _, _) :-
    !.
fix(Fixes, Situation, Module) :-
    last(Fixes, Now-At),
    fix(Situation, Module, Now, At).

fix(_, Module, Now, _) :-
    once(Module:currently(Now, _)),
    !.
fix(Situation, Module, Now, At) :-
    once(retract(Module:currently(Previous, When))),
    !,
    asserta(Module:previously(Previous, When)),
    asserta(Module:currently(Now, At)),
    broadcast(situation(Situation, was(Previous, When))),
    broadcast(situation(Situation, was(Previous, When), now(Now, At))),
    broadcast(situation(Situation, now(Now, At))).
fix(Situation, Module, Now, At) :-
    asserta(Module:currently(Now, At)),
    broadcast(situation(Situation, now(Now, At))).

canny:situation(Situation, retract(When)) :-
    situation_module(Situation, Module),
    retract(Situation, Module, When).

retract(Situation, Module, When0) :-
    forall((   Module:was(Was, When),
               When < When0
           ), once(retract(Module:was(Was, When)))),
    broadcast(situation(Situation, retract(When0))).

canny:situation(Situation, retract(When, Delay)) :-
    number(Delay),
    situation_module(Situation, Module),
    retract(Situation, Module, When, Delay).

retract(Situation, Module, When, Delay) :-
    var(When),
    !,
    once(Module:was(_, When)),
    retract(Situation, Module, When, Delay).
retract(Situation, Module, When, Delay) :-
    When0 is When - Delay,
    retract(Situation, Module, When0).

situation_property(Situation, Property) :-
    canny:situation_property(Situation, Property).

canny:situation_property(Situation, module(Module)) :-
    situation_module(Situation, Module).
canny:situation_property(Situation, defined) :-
    situation_module(Situation, _).
canny:situation_property(Situation, currently(Current, When)) :-
    situation_module(Situation, Module),
    once(Module:currently(Current, When)).
canny:situation_property(Situation, previously(Previous, When)) :-
    situation_module(Situation, Module),
    once(Module:previously(Previous, When)).
