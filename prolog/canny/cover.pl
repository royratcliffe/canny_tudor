:- module(canny_cover,
          [ coverages_by_module/2,      % :Goal,-Coverages:dict
            coverage_for_modules/4      % :Goal,+Modules,-Module,-Coverage
          ]).
:- autoload(library(apply), [convlist/3]).
:- autoload(library(strings), [string_lines/2]).
:- autoload(library(prolog_coverage), [show_coverage/1]).
:- autoload(library(yall), [(>>)/4]).
:- autoload(library(dcg/basics), [whites/2, integer/3, number/3, string/3]).

%!  coverages_by_module(:Goal, -Coverages:dict) is det.
%
%   Calls Goal within show_coverage/1  while   capturing  the  resulting
%   lines of output; Goal  is  typically   run_tests/0  for  running all
%   loaded tests. Parses the lines for   coverage  statistics by module.
%   Ignores lines that do not represent coverage, and also ignores lines
%   that cover non-module files.  Automatically matches prefix-truncated
%   coverage paths as well as full paths.
%
%   @arg Coverages is a  module-keyed   dictionary  of  sub-dictionaries
%   carrying three keys: clauses, cov and fail.

coverages_by_module(Goal, Coverages) :-
    with_output_to(string(String), show_coverage(Goal)),
    string_lines(String, Lines),
    convlist([Line, Module=coverage{
                               clauses:Clauses,
                               cov:Cov,
                               fail:Fail
                           }]>>
             (   string_codes(Line, Codes),
                 phrase(cover_line(Module, Clauses, Cov, Fail), Codes)
             ), Lines, Data),
    dict_create(Coverages, coverages, Data).

cover_line(Module, Clauses, Cov, Fail) -->
    cover_file(Module),
    whites,
    integer(Clauses),
    whites,
    number(Cov),
    whites,
    number(Fail).

cover_file(Module) -->
    "...",
    !,
    { module_property(Module, file(File)),
      sub_atom(File, _, _, 0, Suffix),
      atom_codes(Suffix, Codes)
    },
    string(Codes).
cover_file(Module) -->
    { module_property(Module, file(File)),
      atom_codes(File, Codes)
    },
    string(Codes).

%!  coverage_for_modules(:Goal, +Modules, -Module, -Coverage) is nondet.
%
%   Non-deterministically finds Coverage dictionaries   for all Modules.
%   Bypasses those modules excluded from   the  required list, typically
%   the list of modules belonging to a particular pack and excluding all
%   system and other supporting modules.

coverage_for_modules(Goal, Modules, Module, Coverage) :-
    coverages_by_module(Goal, Coverages),
    Coverage = Coverages.Module,
    memberchk(Module, Modules).
