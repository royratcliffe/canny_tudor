/*  File:    swi/options.pl
    Author:  Roy Ratcliffe
    Created: Jul 19 2025
    Purpose: Options Helpers

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

:- module(swi_options,
          [ select_options/4    % +Options,+RestOptions0,-RestOptions,+Defaults
          ]).
:- use_module(library(option), [select_option/4]).

%!  select_options(+Options,
%!                 +RestOptions0,
%!                 -RestOptions,
%!                 +Defaults) is det.
%
%   Applies multiple select_option/4  predicate  calls   to  a  list  of
%   Options. Applies the list  of  Options   using  a  list of Defaults.
%   Argument terms from Options unify with RestOptions0.
%
%   Defaults are unbound if not present.   The implementation selects an
%   option's  Default  from   the   given    list   of   Defaults  using
%   select_option/4. Option terms  must  have   one  variable.  This  is
%   because select_option/4's fourth argument is   a single argument. It
%   never unifies with multiple variables even  though it succeeds, e.g.
%   `select_option(a(A, B), [], Rest, 1)` unifies   A  with 1, leaving B
%   unbound.
%
%   There is a naming issue. What to   call  the incoming list of Option
%   arguments and the Options  argument  with   which  the  Option terms
%   unify? One possibility: name the Options argument RestOptions0 since
%   they represent the initial set  of   RestOptions  from which Options
%   select. This clashes with select_option/4's  naming convention since
%   Options  is  the  argument  name  for  RestOptions0's  role  in  the
%   option-selection process. Nevertheless, this   version  follows this
%   renamed argument convention.
%
%   Example:
%
%       ?- select_options([a(A), b(B)], [a(1), b(2), c(3)], Rest, [a(0), b(0)]).
%       Rest = [c(3)],
%       A = 1,
%       B = 2.
%
%   @param Options The list of options to select from.
%   @param RestOptions0 The initial list of remaining options.
%   @param RestOptions The remaining options after selection.
%   @param Defaults The list of default values for options.

select_options(Options, RestOptions0, RestOptions, Defaults) :-
    select_options(Options, RestOptions0, RestOptions, Defaults, _).

select_options([], RestOptions, RestOptions, Defaults, Defaults).
select_options([Option|Options], RestOptions0, RestOptions, Defaults0, Defaults) :-
    copy_term(Option, Option1),
    term_variables(Option1, [Default]),
    select_option(Option1, Defaults0, Defaults1, _),
    select_option(Option, RestOptions0, RestOptions1, Default),
    select_options(Options, RestOptions1, RestOptions, Defaults1, Defaults).
