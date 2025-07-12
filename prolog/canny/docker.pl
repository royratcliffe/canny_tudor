/*  File:    canny/docker.pl
    Author:  Roy Ratcliffe
    Created: Jul 11 2025
    Purpose: Docker API

Copyright (c) 2025, Roy Ratcliffe, Northumberland, United Kingdom

Permission is hereby granted, free of charge,  to any person obtaining a
copy  of  this  software  and    associated   documentation  files  (the
"Software"), to deal in  the   Software  without  restriction, including
without limitation the rights to  use,   copy,  modify,  merge, publish,
distribute, sub-license, and/or sell copies  of   the  Software,  and to
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

:- module(canny_docker,
          []).

/** <module> Canny Docker

*/

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

docker_json(Base, Abs) :-
    file_name_extension(Base, json, Name),
    context_file((..)/docker/Name, Abs, []).

%!  context_file(+Spec, -Abs, +Options) is det.
%
%   Determines the absolute path of a file Spec, resolving it relative to
%   the directory of the current module's source file.
%
%   @param Spec The specification of the file, which can be a relative or
%   absolute path.
%   @param Abs The absolute file path of the specified file.
%   @param Options Additional options for the absolute file name
%   resolution.

context_file(Spec, Abs, Options) :-
    context_module(M),
    module_property(M, file(File)),
    file_directory_name(File, Directory),
    absolute_file_name(Spec, Abs, [relative_to(Directory)|Options]).
