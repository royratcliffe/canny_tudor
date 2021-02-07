:- module(swi_options,
          [ select_options/4
          ]).
:- autoload(library(option), [select_option/4]).

%!  select_options(+Options,
%!                 +RestOptions0,
%!                 -RestOptions,
%!                 +Defaults) is det.

select_options(Options, RestOptions0, RestOptions, Defaults) :-
    select_options(Options, RestOptions0, RestOptions, Defaults, _).

select_options([], RestOptions, RestOptions, Defaults, Defaults).
select_options([Option|Options], RestOptions0, RestOptions, Defaults0, Defaults) :-
    copy_term(Option, Option1),
    term_variables(Option1, [Default]),
    select_option(Option1, Defaults0, Defaults1, _),
    select_option(Option, RestOptions0, RestOptions1, Default),
    select_options(Options, RestOptions1, RestOptions, Defaults1, Defaults).
