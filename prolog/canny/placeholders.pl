/*  File:    canny/placeholders.pl
    Author:  Roy Ratcliffe
    Created: Jul 19 2025
    Purpose: Formatting Placeholders
*/

:- module(canny_placeholders,
          []).
:- autoload(library(lists), [append/3]).
:- autoload(library(option), [option/2]).
:- autoload(library(dcg/basics), [string_without//2]).

format_placeholders(Terms, Options) -->
    format_placeholders([], Terms, [], Options).

%!  format_placeholders(+Terms0, -Terms, +Options0, -Options)// is semidet.
%
%   Processes a format string with placeholders using a list of terms and options.
%   Scans the input, replacing placeholders of the form {name} with values from
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
          % See the commented-out code below. However, it is more apropos to
          % append the new option to the end of the options list. This allows
          % the new option to appear after any existing options that may have
          % been specified in the Options0 list.
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
