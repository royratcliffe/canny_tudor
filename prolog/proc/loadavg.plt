:- begin_tests(proc_loadavg).

:- use_module(loadavg).

:- if(current_prolog_flag(windows, true)).
test(loadavg, [error(existence_error(source_sink, '/proc/loadavg'))]) :-
    loadavg(_, _, _, _, _).
:- else.
test(loadavg) :-
    loadavg(A, B, C, D/E, F),
    number(A),
    number(B),
    number(C),
    integer(D),
    integer(E),
    D =< E,
    integer(F).
:- endif.

:- end_tests(proc_loadavg).
