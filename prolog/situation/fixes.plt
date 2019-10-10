:- begin_tests(situation_fixes).

:- use_module(properties).
:- use_module(options).
:- use_module(fixes).

test(situations, []) :-
    situation_options(x, [module(M)]),
    forall(member(Name, [now, was, currently, previously]),
           (   Term =.. [Name, _, _],
               retractall(M:Term)
           )),
    situation_property(x, history([])),
    situation_now(x, a, 0),
    situation_fix(x),
    situation_property(x, history([a-0])),
    situation_property(x, currently(a)),
    \+ situation_property(x, previously(_)),
    situation_now(x, b, 1),
    situation_fix(x),
    situation_property(x, history([b-1, a-0])),
    situation_property(x, currently(b)),
    situation_property(x, previously(a)),
    situation_now(x, c, 2),
    situation_fix(x),
    situation_property(x, history([c-2, b-1, a-0])),
    situation_property(x, currently(c)),
    situation_property(x, previously(b)).

:- end_tests(situation_fixes).
