:- module(proc_loadavg,
          [ loadavg/5
          ]).
:- use_module(library(dcg/basics)).
:- use_module(library(pure_input)).

loadavg(A, B, C, D/E, F) -->
    number(A), whites,
    number(B), whites,
    number(C), whites,
    integer(D), "/",
    integer(E), whites,
    integer(F), blanks_to_nl.

loadavg(A, B, C, D, E) :-
    phrase_from_file(loadavg(A, B, C, D, E), '/proc/loadavg').
