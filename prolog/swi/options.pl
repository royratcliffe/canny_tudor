:- module(swi_options,
          [ select_options/4    % +Options,+RestOptions0,-RestOptions,+Defaults
          ]).
:- autoload(library(option), [select_option/4]).

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

select_options(Options, RestOptions0, RestOptions, Defaults) :-
    select_options(Options, RestOptions0, RestOptions, Defaults, _).

select_options([], RestOptions, RestOptions, Defaults, Defaults).
select_options([Option|Options], RestOptions0, RestOptions, Defaults0, Defaults) :-
    copy_term(Option, Option1),
    term_variables(Option1, [Default]),
    select_option(Option1, Defaults0, Defaults1, _),
    select_option(Option, RestOptions0, RestOptions1, Default),
    select_options(Options, RestOptions1, RestOptions, Defaults1, Defaults).
