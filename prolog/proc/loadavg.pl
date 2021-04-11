:- module(proc_loadavg,
          [ loadavg//5,
            loadavg/5
          ]).
:- use_module(library(dcg/basics)).
:- use_module(library(pure_input)).

%!  loadavg(-Avg1, -Avg5, -Avg15,
%!          -RunnablesRatio, -LastPID)// is semidet.
%
%   Parses the Linux `/proc/loadavg`  process   pseudo  file.  One space
%   separates  all  fields  except  the  runnable  processes  and  total
%   processes, a forward slash separates these two figures.

loadavg(Avg1, Avg5, Avg15, Runnables/Processes, LastPID) -->
    number(Avg1), whites,
    number(Avg5), whites,
    number(Avg15), whites,
    integer(Runnables), "/",
    integer(Processes), whites,
    integer(LastPID), blanks_to_nl.

%!  loadavg(-Avg1, -Avg5, -Avg15,
%!          -RunnablesRatio, -LastPID) is det.
%
%   Captures and parses the current processor load average statistics on
%   Unix systems. Does *not* work on Windows systems.

loadavg(Avg1, Avg5, Avg15,
        RunnablesRatio, LastPID) :-
    phrase_from_file(loadavg(Avg1, Avg5, Avg15,
                             RunnablesRatio, LastPID), '/proc/loadavg').
