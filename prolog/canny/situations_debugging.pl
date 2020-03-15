:- module(canny_situations_debugging, []).

listen :-
    unlisten,
    debug(situation),
    listen(situation(A), situation([A])),
    listen(situation(A, B), situation([A, B])),
    listen(situation(A, B, C), situation([A, B, C])).

unlisten :-
    context_module(M),
    unlisten(M),
    nodebug(situation).

situation([_, fix]) :- !.
situation(Arguments) :-
    Term =.. [situation|Arguments],
    debug(situation, '~q', [Term]).

:- initialization listen.
