:- module(proc_handlers, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(loadavg).

:- http_handler(root(proc/loadavg), proc_loadavg, []).

proc_loadavg(_Request) :-
    loadavg(A, B, C, D/E, F),
    reply_json_dict(_{a:A, b:B, c:C, d:D, e:E, f:F}).
