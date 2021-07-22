/*  File:    swi/zip.pl
    Author:  Roy Ratcliffe
    Created: Jul 22 2021
    Purpose: SWI Zip Files

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

:- module(swi_zip,
          [ zip_file_info/4,                     % +File,-Name,-Attrs,-Zipper
            zipper_codes/3                       % +Zipper,-Codes,+Options
          ]).
:- autoload(library(readutil), [read_stream_to_codes/2]).
:- autoload(library(zip), [zip_open/4, zip_close/1, zipper_file_info/3]).

%!  zip_file_info(+File, -Name, -Attrs, -Zipper) is nondet.
%
%   Non-deterministically walks through the members of a zip File,
%   moving the Zipper current member. It does _not_ read the contents of
%   the zip members, by design. You can use the Name argument to select
%   a member or members before reading.
%
%   @arg Zipper unifies with the open Zipper for reading using
%   zipper_codes/3 or zipper_open_current/3.

zip_file_info(File, Name, Attrs, Zipper) :-
    setup_call_cleanup(
        zip_open(File, read, Zipper, []),
        (   zipper_goto(Zipper, first),
            file_info(Zipper, Name, Attrs)
        ),
        zip_close(Zipper)
    ).

file_info(Zipper, Name, Attrs) :- zipper_file_info(Zipper, Name, Attrs).
file_info(Zipper, Name, Attrs) :-
    zipper_goto(Zipper, next),
    file_info(Zipper, Name, Attrs).

%!  zipper_codes(+Zipper, -Codes, +Options) is semidet.
%
%   Reads the current Zipper file as Codes. Options may be:
%
%       - encoding(utf8) for UTF-8 encoded text, or
%       - type(binary) for binary octets, and so on.

zipper_codes(Zipper, Codes, Options) :-
    setup_call_cleanup(
        zipper_open_current(Zipper, Stream, Options),
        read_stream_to_codes(Stream, Codes),
        close(Stream)
    ).
