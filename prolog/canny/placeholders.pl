/*  File:    canny/placeholders.pl
    Author:  Roy Ratcliffe
    Created: Jul 19 2025
    Purpose: Formatting Placeholders
*/

:- module(canny_placeholders,
          []).
:- autoload(library(lists), [append/3]).
:- autoload(library(option), [option/2]).
:- autoload(library(dcg/basics), [string_without/4]).

format_path(Terms, Options) -->
    format_path([], Terms, [], Options).

%!  format_path(+Atomics0, -Atomics, +Options0, -Options)// is semidet.
%
%   Formats a path by processing a list of atomics and options. The atomics
%   are processed to construct a path, and the options are used to
%   replace placeholders in the path. The predicate constructs a list of
%   atoms representing the formatted path, and returns the updated list
%   of options.
%
%   The predicate scans the input atomics and options, replacing any
%   placeholders in the format string with the corresponding values from
%   the options list. It constructs the path by concatenating the atoms
%   in the atomics list, and returns the updated options list with any
%   options that were used to replace placeholders.
%
%   The predicate uses DCG rules to process the input atomics and options,
%   allowing for flexible and dynamic path construction. It handles
%   placeholders in the format string, ensuring that the resulting path
%   is correctly formatted according to the specified options.
%
%   The predicate is designed to be used in conjunction with the Docker API
%   operations, where the format string represents the path for a specific
%   operation, and the options provide the necessary values to replace
%   the placeholders in the path. The resulting path can be used with the
%   HTTP client to make requests to the Docker API.
%
%   @param Atomics0 The initial list of atomics to be processed.
%   @param Atomics The resulting list of atomics after processing.
%   @param Options0 The initial list of options to be processed.
%   @param Options The resulting list of options after processing.

format_path(Atomics0, Atomics, Options0, Options) -->
    "{",
    string_without("}", NameCodes),
    "}",
    !,
    { atom_codes(Name, NameCodes),
      Option =.. [Name, Value],
      (   option(Option, Options0)
      ->  Options_ = Options0
      ;   % Prepending the new option would be possible.
          % See the commented-out code below.
          % However, it is more apropos to append
          % the new option to the end of the options list.
          % This allows the new option to appear after
          % any existing options that may have been
          % specified in the Options0 list.
          %
          % Options_ = [Option|Options0]
          append(Options0, [Option], Options_)
      ),
      append(Atomics0, [Value], Atomics_)
    },
    format_path(Atomics_, Atomics, Options_, Options).
format_path(Atomics0, Atomics, Options0, Options) -->
    string_without("{", Codes),
    (   { Codes == []
        }
    ->  { Atomics = Atomics0,
          Options = Options0
        }
    ;   { atom_codes(Atom, Codes),
          append(Atomics0, [Atom], Atomics_)
        },
        format_path(Atomics_, Atomics, Options0, Options)
    ).
