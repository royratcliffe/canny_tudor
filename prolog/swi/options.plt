:- begin_tests(swi_options).

:- use_module(options).

test(select_options, [true(var(A))]) :-
    select_options([term(A)], [], [], []).
test(select_options, [true(A==x)]) :-
    select_options([term(A)], [], [], [term(x)]).

:- end_tests(swi_options).
