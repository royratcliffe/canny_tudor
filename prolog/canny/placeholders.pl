/*  File:    canny/placeholders.pl
    Author:  Roy Ratcliffe
    Created: Jul 19 2025
    Purpose: Formatting Placeholders

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

:- module(canny_placeholders,
          [ format_placeholders/3,              % +Format, -Atom, +Options
            placeholders//2,
            placeholders//4
          ]).
:- autoload(library(error), [must_be/2]).
:- autoload(library(lists), [append/3]).
:- autoload(library(option), [select_option/3]).
:- autoload(library(dcg/basics), [string_without/4]).

%!  format_placeholders(+Format, -Atom, +Options) is det.
%!  format_placeholders(+Format, -Atom, +Options, -RestOptions) is det.
%
%   Formats a string with placeholders in the form of `{name}`. The
%   placeholders are replaced with corresponding values from the options
%   list. The result is an atom with the formatted string.
%
%   The Format string can be any atom or string containing placeholders.
%   The Options list should contain terms of the form `name(Value)`, where
%   `name` is the placeholder name and `Value` is the value to replace it
%   with. If a placeholder does not have a corresponding value in the
%   Options list, it will not be replaced, and the placeholder will remain
%   in the resulting atom.
%
%   @param Format The format string containing placeholders.
%   @param Atom The resulting atom with placeholders replaced.
%   @param Options The list of options containing values for placeholders.
%   @param RestOptions The remaining options after processing the placeholders.

format_placeholders(Format, Atom, Options) :-
    format_placeholders(Format, Atom, Options, _).

format_placeholders(Format, Atom, Options, RestOptions) :-
    atom_codes(Format, Codes),
    phrase(placeholders([], Terms, Options, RestOptions), Codes),
    must_be(ground, Terms),
    atomic_list_concat(Terms, '', Atom).

%!  placeholders(-Terms, ?Options)// is det.
%
%   Formats a list of terms by replacing placeholders in the form of `{name}`
%   with corresponding values from the options list. The placeholders are
%   replaced with the values associated with the names in the options list.
%
%   The result is a list of atoms and values, and a completed options list.
%
%   @param Terms The list of terms to be formatted.
%   @param Options The list of options containing values for placeholders.

placeholders(Terms, Options) -->
    placeholders([], Terms, [], Options).

%!  placeholders(+Terms0, -Terms, +Options0, -Options)// is det.
%
%   Processes a format string with placeholders using a list of terms and options.
%   The format string is the difference list of codes, where placeholders are
%   replaced with values from the options list. The result is a list of terms
%   and an updated options list.
%
%   Scans the input, replacing placeholders of the form `{name}` with values from
%   the options list. The result is a list of atoms and values, and an updated
%   options list. Uses DCG rules for flexible parsing and substitution.
%
%   The resulting list of terms contains atoms and values, where each placeholder
%   is replaced with the corresponding value from the options list. The options
%   list is updated to include any new options found in the format string.
%
%   @param Terms0 The initial list of terms to be processed.
%   @param Terms The resulting list of terms after processing.
%   @param Options0 The initial list of options to be processed.
%   @param Options The resulting list of options after processing.

placeholders(Terms0, Terms, Options0, Options) -->
    "{",
    string_without("}", NameCodes),
    "}",
    !,
    { atom_codes(Name, NameCodes),
      Option =.. [Name, Value],
      (   select_option(Option, Options0, Options_)
      ->  true
      ;   % Prepending the new option would be possible.
          % See the commented-out code below. However, it is more apropos to
          % append the new option to the end of the options list. This allows
          % the new option to appear after any existing options that may have
          % been specified in the Options0 list.
          % Delegate to the merge_options/3 predicate to handle the
          % appending of the new option to the existing options list.
          %
          % Options_ = [Option|Options0]
          % append(Options0, [Option], Options_)
          merge_options([Option], Options0, Options_)
      ),
      append(Terms0, [Value], Terms_)
    },
    placeholders(Terms_, Terms, Options_, Options).
placeholders(Terms0, Terms, Options0, Options) -->
    string_without("{", Codes),
    (   { Codes == []
        }
    ->  { Terms = Terms0,
          Options = Options0
        }
    ;   { atom_codes(Atom, Codes),
          append(Terms0, [Atom], Terms_)
        },
        placeholders(Terms_, Terms, Options0, Options)
    ).
