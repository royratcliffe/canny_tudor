:- begin_tests(swi_streams).

:- use_module(streams).

test(no_streams) :-
    close_streams([], []).

test(already_closed,
     [   setup(open_null_stream(Null)),
         Catcher = error(existence_error(stream, _),
                         context(system:close/1, 'already closed'))
     ]) :-
    close_streams([Null, Null], [Catcher]).

:- end_tests(swi_streams).
