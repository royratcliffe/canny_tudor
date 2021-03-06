:- module(proc_loadavg,
          [ loadavg//5,
            loadavg/5
          ]).
:- use_module(library(dcg/basics)).
:- use_module(library(pure_input)).

%!  loadavg(-Avg1, -Avg5, -Avg15,
%!          -RunnablesRatio, -LastPID)// is semidet.
%
%   Parses the Linux =|/proc/loadavg|= process   pseudo-file.  One space
%   separates  all  fields  except  the  runnable  processes  and  total
%   processes, a forward slash separates these two figures.
%
%   Load-average statistics comprise: three floating  point numbers, one
%   integer ratio and one process identifier.
%
%       - Load average for last minute
%       - Load average for last five minutes
%       - Load average for last 15 minutes
%       - Number of currently-runnable processes, meaning either
%         actually running or ready to run
%       - Total number of processes
%       - Last created process identifier
%
%   It follows logically that runnable processes  is always less than or
%   equal to total processes.
%
%   One space separates all fields  except   the  runnable processes and
%   total processes, a forward slash separates   these  two figures. The
%   implementation applies this  requirement   explicitly.  The  grammar
%   fails if more than one space  exists,   or  if finds the terminating
%   newline missing. This approach allows you  to reverse the grammar to
%   generate the load-average codes from the load-average figures.

loadavg(Avg1, Avg5, Avg15, Runnables/Processes, LastPID) -->
    number(Avg1), " ",
    number(Avg5), " ",
    number(Avg15), " ",
    integer(Runnables), "/",
    integer(Processes), " ",
    integer(LastPID), "\n".

%!  loadavg(-Avg1, -Avg5, -Avg15,
%!          -RunnablesRatio, -LastPID) is det.
%
%   Captures and parses the current processor load average statistics on
%   Linux systems. Does *not* work on Windows systems.
%
%   @throws existence_error(source_sink, '/proc/loadavg') on Windows, or
%   other operating systems that do not have a `proc` subsystem.

loadavg(Avg1, Avg5, Avg15,
        RunnablesRatio, LastPID) :-
    phrase_from_file(loadavg(Avg1, Avg5, Avg15,
                             RunnablesRatio, LastPID), '/proc/loadavg').
