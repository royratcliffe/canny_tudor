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

format_placeholders(Terms, Options) -->
    format_placeholders([], Terms, [], Options).

%!  format_placeholders(+Terms0, -Terms, +Options0, -Options)// is semidet.
%
%   Formats a placeholders by processing a list of terms and options. The terms
%   are processed to construct a placeholders, and the options are used to
%   replace placeholders in the placeholders. The predicate constructs a list of
%   atoms representing the formatted placeholders, and returns the updated list
%   of options.
%
%   The predicate scans the input terms and options, replacing any
%   placeholders in the format string with the corresponding values from
%   the options list. It constructs the placeholders by concatenating the atoms
%   in the terms list, and returns the updated options list with any
%   options that were used to replace placeholders.
%
%   The predicate uses DCG rules to process the input terms and options,
%   allowing for flexible and dynamic placeholders construction. It handles
%   placeholders in the format string, ensuring that the resulting placeholders
%   is correctly formatted according to the specified options.
%
%   The predicate is designed to be used in conjunction with the Docker API
%   operations, where the format string represents the placeholders for a specific
%   operation, and the options provide the necessary values to replace
%   the placeholders in the placeholders. The resulting placeholders can be used with the
%   HTTP client to make requests to the Docker API.
%
%   @param Terms0 The initial list of terms to be processed.
%   @param Terms The resulting list of terms after processing.
%   @param Options0 The initial list of options to be processed.
%   @param Options The resulting list of options after processing.

format_placeholders(Terms0, Terms, Options0, Options) -->
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
      append(Terms0, [Value], Terms_)
    },
    format_placeholders(Terms_, Terms, Options_, Options).
format_placeholders(Terms0, Terms, Options0, Options) -->
    string_without("{", Codes),
    (   { Codes == []
        }
    ->  { Terms = Terms0,
          Options = Options0
        }
    ;   { atom_codes(Atom, Codes),
          append(Terms0, [Atom], Terms_)
        },
        format_placeholders(Terms_, Terms, Options0, Options)
    ).
