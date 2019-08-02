:- module(os_apps_debugging, []).

:- use_module(apps, []).

:- multifile user:do/1.
:- public user:do/1.

user:do(debug(os_apps)) :-
    user:do(nodebug(os_apps)),
    debug(os_apps),
    listen(os:app_started(App), started(App)),
    listen(os:app_stopped(App, Status), stopped(App, Status)),
    listen(os:app_decoded(App, Codes), decoded(App, Codes)).

user:do(nodebug(os_apps)) :-
    context_module(Module),
    unlisten(Module),
    nodebug(os_apps).

started(App) :-
    debug(os_apps, 'started ~p', [App]).

stopped(App, Status) :-
    debug(os_apps, 'stopped ~p ~p', [App, Status]).

decoded(App, Codes) :-
    Codes =.. [Name, Codes0],
    debug(os_apps, 'decoded ~p ~s ~s', [App, Name, Codes0]).
