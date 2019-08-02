:- module(os_apps_testing, []).

:- use_module(apps, []).

:- multifile os:property_for_app/2.

os:property_for_app(path(path(mspaint)), mspaint) :-
    !.
