:- module(proc_loadavg,
          [ loadavg/5
          ]).
:- use_module(library(dcg/basics)).
:- use_module(library(pure_input)).

loadavg(Avg1, Avg5, Avg15, Runnables/Processes, LastPID) -->
    number(Avg1), whites,
    number(Avg5), whites,
    number(Avg15), whites,
    integer(Runnables), "/",
    integer(Processes), whites,
    integer(LastPID), blanks_to_nl.

loadavg(Avg1, Avg5, Avg15, RunnablesRatio, LastPID) :-
    phrase_from_file(loadavg(Avg1, Avg5, Avg15, RunnablesRatio, LastPID), '/proc/loadavg').
