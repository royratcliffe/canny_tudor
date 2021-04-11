:- begin_tests(proc_loadavg).

:- use_module(loadavg).

test(loadavg) :-
    loadavg(A, B, C, D/E, F),
    number(A),
    number(B),
    number(C),
    integer(D),
    integer(E),
    integer(F).

:- end_tests(proc_loadavg).
