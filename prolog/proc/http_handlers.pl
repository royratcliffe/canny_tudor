:- module(proc_http_handlers, []).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(loadavg).

:- http_handler(root(proc/loadavg), proc_loadavg, []).

proc_loadavg(_Request) :-
    loadavg(A, B, C, D/E, F),
    reply_json_dict(_{avg1:A,
                      avg5:B,
                      avg15:C,
                      runnables:D,
                      processes:E,
                      last_pid:F
                     }).
