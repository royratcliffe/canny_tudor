/*  File:    canny/docker.pl
    Author:  Roy Ratcliffe
    Created: Jul 11 2025
    Purpose: Docker API
*/

:- module(canny_docker,
          []).

/** <module> Canny Docker

*/

%!  read_stream_to_codes_until_end_of_file(+In, -Codes) is nondet.
%!  read_stream_to_codes_until(+In, -Codes, +Until) is nondet.
%
%   Reads from a stream and finds a specific code sequence. The
%   predicate reads the stream until it reaches the end or encounters
%   the specified code term.
%
%   @param In The input stream to read from.
%   @param Codes The codes read from the stream.
%   @param Until The code term that terminates the reading.

read_stream_to_codes_until_end_of_file(In, Codes) :-
    read_stream_to_codes_until(In, Codes, end_of_file).

read_stream_to_codes_until(In, Codes, Until) :-
    repeat,
    (   true,
        read_stream_to_codes(In, Codes),
        Codes \== Until
    ->  true
    ;   !,
        fail
    ).
