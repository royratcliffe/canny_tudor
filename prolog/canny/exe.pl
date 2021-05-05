/*  File:    canny/exe.pl
    Author:  r0y
    Created: May  5 2021
    Purpose: Canny Executables
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

%!  exe(+Executable, +Arguments, +Options) is semidet.
%
%   Implements an experimental approach to process_create/3 using
%   concurrent/3. It operates concurrent pipe reads, pipe writes
%   and process waits. New Options terms offer additional pipe streaming
%   arguments. See fully-enumerated list below.
%
%       * stdin(codes(Codes))
%       * stdin(atom(Atom))
%       * stdin(string(String))
%       * stdout(codes(Codes))
%       * stdout(atom(Atom))
%       * stdout(string(String))
%       * stderr(codes(Codes))
%       * stderr(atom(Atom))
%       * stderr(string(String))
%       * status(Status)
%
%   If Options specifies any of these terms, process goals prepare to
%   write, read and wait concurrently as necessary. This implies that
%   reading standard output and waiting for the process status happens
%   at the same time. Same goes for writing to standard input. The
%   number of concurrent threads exactly matches the number of
%   concurrent process goals.
%
%   Important to close the input stream immediately after writing and
%   during the call phase. Do *not* wait for the clean-up phase to close
%   the input stream, otherwise the process will never terminate. It
%   will hang while waiting for standard input to close, assuming it
%   reads the input.
%
%   This leads to a key caveat when using a single concurrent thread.
%   A single callee thread executes the primary read-write goals in
%   sequential order. The current implementation preserves the Options
%   ordering. Hence output should always preceed input, i.e. writing to
%   standard input should go first before attempting to read from
%   standard output. Otherwise the sequence will block indefinitely. For
%   this reason, the number of concurrent threads matches the number of
%   concurrent goals. This abviates the sequencing of the goals because
%   all goals implicitly execute concurrently.

exe(Executable, Arguments, Options) :-
    exe(Options, Options_, Calls, Cleanups),
    length(Calls, Threads),
    setup_call_cleanup(
        process_create(Executable, Arguments, Options_),
        concurrent(Threads, Calls, []),
        concurrent(Threads, Cleanups, [])).

exe([], [], [], []).
exe([Option0|Options0], [Option|Options], [Call|Calls], [Cleanup|Cleanups]) :-
    opt(Option0, Option, Call, Cleanup),
    !,
    exe(Options0, Options, Calls, Cleanups).
exe([Option|Options0], [Option|Options], Calls, Cleanups) :-
    exe(Options0, Options, Calls, Cleanups).

opt(stdin(Compound), stdin(pipe(Stream)),
    (   format(Stream, '~s', [S]),
        close(Stream)
    ), true) :-
    compound(Compound),
    compound_name_arguments(Compound, Name, [S]),
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
