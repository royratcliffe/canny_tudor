/*  File:    in/temporary.pl
    Author:  Roy Ratcliffe
    Created: Nov 23 2021
    Purpose: In Temporary Module

Copyright (c) 2021, Roy Ratcliffe, United Kingdom

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

:- module(in_temporary,
          [ ask_in_temporary/3                  % +Codes,-Heads,+Options
          ]).
:- autoload(library(modules), [in_temporary_module/3]).
:- autoload(library(charsio), [open_chars_stream/2]).
:- autoload(library(sandbox), [safe_call/1]).

%!  ask_in_temporary(+Codes, -Heads, +Options) is det.
%
%   Unifies with all public Heads within the given source Codes using a
%   temporary module. Cleans up the module on success, or failure.
%
%   Uses the temporary module atom as the load_files/3 stream
%   identifier. It must be different every time, otherwise the stream
%   will not successfully load.
%
%   Not all the heads are necessarily ground terms; though the loader
%   warns about singletons. You can filter out non-grounds using
%   include/3, example below.
%
%       ask_in_temporary(':- public ask/1. ask(_).', A, []),
%           include(ground, A, B).

ask_in_temporary(Codes, Heads, Options) :-
    in_temporary_module(
        M,
        setup_call_cleanup(
            open_chars_stream(Codes, Stream),
            load_files(M:M, [stream(Stream), module(M)|Options]),
            close(Stream)
        ),
        findall(H, predicate_property(M:H, public) -> safe_call(M:H), Heads)
    ).
