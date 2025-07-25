/*  File:    read/until.pl
    Author:  Roy Ratcliffe
    Created: Jul 12 2025
    Purpose: Read Until
*/

:- module(read_until,
          [ read_stream_to_codes_until_end_of_file/2, % +In, -Codes
            read_stream_to_codes_until/3              % +In, -Codes, +Until
          ]).
:- autoload(library(readutil), [read_stream_to_codes/2]).

%!  read_stream_to_codes_until_end_of_file(+In, -Codes) is nondet.
%!  read_stream_to_codes_until(+In, -Codes, +Until) is nondet.
%
%   Reads Codes from a stream until it  finds a specific code term, such
%   as `end_of_file`. The predicate reads the stream until it encounters
%   the `Until` code term, which defaults  to `end_of_file`. It succeeds
%   non-deterministically  for  each  chunk  read  before  reaching  the
%   `Until` code term. Use this predicate   to process multiple messages
%   or data chunks from a stream,   handling  each chunk separately. The
%   `Codes` variable contains the codes read   from  the stream, and the
%   predicate succeeds until it reaches the `Until` condition.
%
%   @param In The input stream to read from.
%   @param Codes The codes read from the stream.
%   @param Until The code term that terminates the reading.

read_stream_to_codes_until_end_of_file(In, Codes) :-
    read_stream_to_codes_until(In, Codes, end_of_file).

read_stream_to_codes_until(In, Codes, Until) :-
    % Use repeat/0 to allow backtracking for each chunk read until `Until` is
    % encountered. The cut (!) and fail ensure the predicate only succeeds for
    % valid chunks and stops at `Until`.
    repeat,
    (   read_stream_to_codes(In, Codes),
        Codes \== Until
    ->  true
    ;   % Cut (!) and fail here to terminate the repeat loop when
        % `Until` is encountered.
        !,
        fail
    ).
