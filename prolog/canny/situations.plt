:- begin_tests(canny_situations).

:- use_module(situations).

test(situations, []) :-
    for_situation(module(M), x),
    forall(member(Name, [now, was, currently, previously]),
           (   Term =.. [Name, _, _],
               retractall(M:Term)
           )),
    situation_property(x, history([])),
    for_situation(now(a, 0), x),
    for_situation(fix, x),
    situation_property(x, history([was(a, 0)])),
    situation_property(x, currently(a, _)),
    \+ situation_property(x, previously(_, _)),
    for_situation(now(b, 1), x),
    for_situation(fix, x),
    situation_property(x, history([was(b, 1), was(a, 0)])),
    situation_property(x, currently(b, _)),
    situation_property(x, previously(a, _)),
    for_situation(now(c, 2), x),
    for_situation(fix, x),
    situation_property(x, history([was(c, 2), was(b, 1), was(a, 0)])),
    situation_property(x, currently(c, _)),
    situation_property(x, previously(b, _)).

:- end_tests(canny_situations).
