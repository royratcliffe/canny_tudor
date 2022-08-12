/*  File:    canny/z.pl
    Author:  Roy Ratcliffe
    Created: Dec  3 2021
    Purpose: Zipped Files

Copyright (c) 2022, Roy Ratcliffe, Northumberland, United Kingdom

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

:- module(canny_z,
          [ enz/2,                              % +Data:list,+File
            unz/2                               % +File,-Data:list
          ]).
:- autoload(library(memfile), [new_memory_file/1, open_memory_file/4]).
:- autoload(library(zip), [zip_open/4, zip_close/1, zipper_file_info/3]).

%!  enz(+Data:list, +File) is semidet.
%
%   Zips Data to File. Writes zip(Name:atom, Info:dict,
%   MemFile:memory_file) functor triples to File where `Name` is the
%   key; `MemFile` is the content as a memory file. Converts the `Info`
%   dictionary to new-member options when building up the zipper.
%   Ignores any non-valid key pairs, including offset plus compressed
%   and uncompressed sizes.
%
%   The implementation _asserts_ octet encoding for new files with a
%   zipper. The predicate for creating a zipper member does *not* allow
%   for an encoding option. It encodes as binary by default.

enz(Data, File) :-
    setup_call_cleanup(
        zip_open(File, write, Zipper, []),
        enz_(Data, Zipper),
        zip_close(Zipper)
    ).

enz_([H|T], Zipper) :-
    enz__(H, Zipper),
    enz_(T, Zipper).
enz_([], _Zipper).

enz__(zip(Name, Info, File), Zipper) :-
    dict_pairs(Info, zip, Pairs),
    new_file_in_zip_options(Pairs, Options),
    setup_call_cleanup(
        open_memory_file(File, read, In, [encoding(octet)]),
        setup_call_cleanup(
            zipper_open_new_file_in_zip(Zipper, Name, Out, Options),
            (   stream_property(Out, encoding(octet)),
                copy_stream_data(In, Out)
            ),
            close(Out)
        ),
        close(In)
    ).

new_file_in_zip_options([Key-Value|T0], [Option|T]) :-
    new_file_in_zip_option(Key),
    !,
    Option =.. [Key, Value],
    new_file_in_zip_options(T0, T).
new_file_in_zip_options([_|T0], T) :-
    new_file_in_zip_options(T0, T).
new_file_in_zip_options([], []).

new_file_in_zip_option(extra).
new_file_in_zip_option(comment).
new_file_in_zip_option(time).
new_file_in_zip_option(method).
new_file_in_zip_option(level).
new_file_in_zip_option(zip64).

%!  unz(+File, -Data:list) is semidet.
%
%   Unzips File to Data, a list of `zip` functors with `Name` atom,
%   `Info` dictionary and `MemFile` content arguments.
%
%   You cannot apply unz/2 to an empty zip File. A bug crashes the
%   entire Prolog run-time virtual machine.

unz(File, Data) :-
    setup_call_cleanup(
        zip_open(File, read, Zipper, []),
        unz_(Data, Zipper),
        zip_close(Zipper)
    ).

%   Trailing underscores represent prime. The first prime unzips the
%   first zipper member. Prime-prime unzips the next and any subsequent
%   members. Triple prime performs the member-wise opening and reading.

unz_([H|T], Zipper) :-
    zipper_goto(Zipper, first),
    !,
    unz___(H, Zipper),
    unz__(T, Zipper).
unz_([], _Zipper).

unz__([H|T], Zipper) :-
    zipper_goto(Zipper, next),
    !,
    unz___(H, Zipper),
    unz__(T, Zipper).
unz__([], _Zipper).

unz___(zip(Name, Info, File), Zipper) :-
    zipper_file_info(Zipper, Name, Info),
    new_memory_file(File),
    setup_call_cleanup(
        open_memory_file(File, write, Out, [encoding(octet)]),
        setup_call_cleanup(
            zipper_open_current(Zipper, In, [type(binary)]),
            copy_stream_data(In, Out),
            close(In)
        ),
        close(Out)
    ).
