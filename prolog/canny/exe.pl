/*  File:    canny/exe.pl
    Author:  Roy Ratcliffe
    Created: May  5 2021
    Purpose: Canny Executables

Copyright (c) 2021, Roy Ratcliffe, Northumberland, United Kingdom

Permission is hereby granted, free of charge,  to any person obtaining a
copy  of  this  software  and    associated   documentation  files  (the
"Software"), to deal in  the   Software  without  restriction, including
without limitation the rights to  use,   copy,  modify,  merge, publish,
distribute, sublicense, and/or sell  copies  of   the  Software,  and to
permit persons to whom the Software is   furnished  to do so, subject to
the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT  WARRANTY OF ANY KIND, EXPRESS
OR  IMPLIED,  INCLUDING  BUT  NOT   LIMITED    TO   THE   WARRANTIES  OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR   PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS  OR   COPYRIGHT  HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY,  WHETHER   IN  AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM,  OUT  OF   OR  IN  CONNECTION  WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/

:- module(canny_exe,
          [ exe/3                       % +Executable,+Arguments,+Options
          ]).
:- predicate_options(exe/3, 3,
                     [ status(integer),
                       pass_to(process_create/3, 3)
                     ]).
:- autoload(library(process), [process_create/3]).
:- autoload(library(thread), [concurrent/3]).
:- use_module(library(canny/bytes)).

%!  exe(+Executable, +Arguments, +Options) is semidet.
%
%   Implements an experimental  approach   to  wrapping process_create/3
%   using concurrent/3. It operates concurrent   pipe reads, pipe writes
%   and process waits. Predicate parameters   match process_create/3 but
%   with a few minor  but  key   improvements.  New  Options terms offer
%   additional    enhanced    pipe     streaming      arguments.     See
%   partially-enumerated list below.
%
%       * stdin(bytes(Bytes))
%       * stdin(codes(Codes))
%       * stdin(atom(Atom))
%       * stdin(string(String))
%       * stdout(bytes(Bytes))
%       * stdout(codes(Codes))
%       * stdout(atom(Atom))
%       * stdout(string(String))
%       * stderr(codes(Codes))
%       * stderr(atom(Atom))
%       * stderr(string(String))
%       * status(Status)
%
%   If Options specifies any of the above terms, exe/3 prepares goals to
%   write, read and wait concurrently  as   necessary  according  to the
%   required configuration. This implies that   reading  standard output
%   and waiting for the process status happens   at  the same time. Same
%   goes for writing to standard input. The number of concurrent threads
%   therefore exactly matches the number   of  concurrent process goals.
%   This goes for clean-up goals as   well.  Predicate concurrent/3 does
%   not allow zero threads  however;  it   throws  a  `type_error`.  The
%   implementation always assigns at least one   thread which amounts to
%   reusing the calling thread non-concurrently.
%
%   All the `std` terms above can also   take  a stream options list, so
%   can override default encoding on the   process  pipes. The following
%   example illustrates. It sends a friendly "hello" in Mandarin Chinese
%   through the Unix `tee` command which   relays the stream to standard
%   output and tees it off to =|/dev/stderr|= or standard error for that
%   process. Note that exe/3 decodes the   output  and error separately,
%   one as an atom but the other as a string.
%
%       exe(path(tee),
%           [ '/dev/stderr'
%           ],
%           [ stdin(atom(你好, [encoding(utf8)])),
%             stdout(atom(A, [encoding(utf8)])),
%             stderr(string(B, [encoding(utf8)])),
%             status(exit(0))
%           ]).
%
%   ---+++ Implementation Notes
%
%   Important to close the input stream   immediately  after writing and
%   during the call phase. Do *not* wait for the clean-up phase to close
%   the input stream, otherwise the  process   will  never terminate. It
%   will hang while waiting for standard   input  to close, assuming the
%   sub-process reads the input.
%
%   This leads to a key caveat when  using a single concurrent thread. A
%   single callee thread  executes  the   primary  read-write  goals  in
%   sequential order. The current implementation   preserves the Options
%   ordering. Hence output should always preceed  input, i.e. writing to
%   standard input should go  first  before   attempting  to  read  from
%   standard output. Otherwise the sequence will block indefinitely. For
%   this reason, the number of concurrent  threads matches the number of
%   concurrent goals. This abviates the sequencing  of the goals because
%   all goals implicitly execute concurrently.
%
%   @tbd Take care when using the  status(Status) option unless you have
%   stdin(null) on Windows because, for   some  sub-processes, the goals
%   never complete.

exe(Executable, Arguments, Options) :-
    exe(Options, Options_, Calls, Cleanups),
    threads(Calls, CallThreads),
    threads(Cleanups, CleanupThreads),
    setup_call_cleanup(
        process_create(Executable, Arguments, Options_),
        concurrent(CallThreads, Calls, []),
        concurrent(CleanupThreads, Cleanups, [])).

threads(Goals, NumberOfGoals) :-
    length(Goals, NumberOfGoals),
    NumberOfGoals >= 1,
    !.
threads(_, 1).

exe([], [], [], []).
exe([Option0|Options0], [Option|Options], [Call|Calls], [Cleanup|Cleanups]) :-
    opt(Option0, Option, Call, Cleanup),
    !,
    exe(Options0, Options, Calls, Cleanups).
exe([Option|Options0], [Option|Options], Calls, Cleanups) :-
    exe(Options0, Options, Calls, Cleanups).

opt(stdin(bytes(Bytes)), stdin(pipe(Stream, [type(binary), encoding(octet)])),
    (   set_stream(Stream, type(binary)),
        put_bytes(Stream, Bytes),
        close(Stream)
    ), true) :-
    !.
opt(stdin(Compound), stdin(pipe(Stream)),
    (   format(Stream, '~s', [S]),
        close(Stream)
    ), true) :-
    compound(Compound),
    compound_name_arguments(Compound, Name, [S]),
    s(Name),
    !.
opt(stdin(Compound), stdin(pipe(Stream, Options)),
    (   format(Stream, '~s', [S]),
        close(Stream)
    ), true) :-
    compound(Compound),
    compound_name_arguments(Compound, Name, [S, Options]),
    s(Name),
    !.
opt(Compound0, Compound, Call, Cleanup) :-
    compound(Compound0),
    compound_name_arguments(Compound0, Name, [Argument0]),
    std(Name),
    !,
    std(Argument0, Argument, Call, Cleanup),
    compound_name_arguments(Compound, Name, [Argument]).
opt(status(Status), process(PID), process_wait(PID, Status), true).

s(codes).
s(atom).
s(string).

std(stdout).
std(stderr).

std(bytes(Bytes), pipe(Stream, [type(binary), encoding(octet)]),
    (   set_stream(Stream, type(binary)),
        read_stream_to_bytes(Stream, Bytes)
    ), close(Stream)).
std(codes(Codes), pipe(Stream),
    read_stream_to_codes(Stream, Codes), close(Stream)).
std(atom(Atom), pipe(Stream),
    (   read_stream_to_codes(Stream, Codes),
        atom_codes(Atom, Codes)
    ), close(Stream)).
std(string(String), pipe(Stream),
    (   read_stream_to_codes(Stream, Codes),
        string_codes(String, Codes)
    ), close(Stream)).
std(codes(Codes, Options), pipe(Stream, Options),
    read_stream_to_codes(Stream, Codes), close(Stream)).
std(atom(Atom, Options), pipe(Stream, Options),
    (   read_stream_to_codes(Stream, Codes),
        atom_codes(Atom, Codes)
    ), close(Stream)).
std(string(String, Options), pipe(Stream, Options),
    (   read_stream_to_codes(Stream, Codes),
        string_codes(String, Codes)
    ), close(Stream)).
