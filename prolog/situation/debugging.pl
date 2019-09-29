:- module(situation_debugging, []).

:- public listen/0.

listen :-
    unlisten,
    debug(situation),
    listen(situation:Term, situation(Term)).

unlisten :-
    context_module(M),
    unlisten(M),
    nodebug(situation).

situation(Term) :-
    debug(situation, '~q', [Term]).

:- initialization listen.
