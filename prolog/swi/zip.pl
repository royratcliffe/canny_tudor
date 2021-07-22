/*  File:    swi/zip.pl
    Author:  Roy Ratcliffe
    Created: Jul 22 2021
    Purpose: SWI Zip Files
*/

:- module(zip,
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
%   Reads the current Zipper file as codes. Options may be:
%
%       - encoding(utf8) for UTF-8 encoded text, or
%       - type(binary) for binary octets, and so on.

zipper_codes(Zipper, Codes, Options) :-
    setup_call_cleanup(
        zipper_open_current(Zipper, Stream, Options),
        read_stream_to_codes(Stream, Codes),
        close(Stream)
    ).
