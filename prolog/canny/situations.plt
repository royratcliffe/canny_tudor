:- begin_tests(canny_situations).

:- use_module(situations).

test(situations, []) :-
    situation(x, module(M)),
    forall(member(Name, [now, was, currently, previously]),
           (   Term =.. [Name, _, _],
               retractall(M:Term)
           )),
    situation_property(x, history([])),
    situation(x, now(a, 0)),
    situation(x, fix),
    situation_property(x, history([was(a, 0)])),
    situation_property(x, currently(a, _)),
    \+ situation_property(x, previously(_, _)),
    situation(x, now(b, 1)),
    situation(x, fix),
    situation_property(x, history([was(b, 1), was(a, 0)])),
    situation_property(x, currently(b, _)),
    situation_property(x, previously(a, _)),
    situation(x, now(c, 2)),
    situation(x, fix),
    situation_property(x, history([was(c, 2), was(b, 1), was(a, 0)])),
    situation_property(x, currently(c, _)),
    situation_property(x, previously(b, _)).

:- end_tests(canny_situations).
