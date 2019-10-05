:- module(situation_options, [situation_options/2]).

:- meta_predicate situation_options(:, ?).

:- predicate_options(situation_options/2, 2,
                     [   module(atom),
                         pass_to((dynamic)/2, 2)
                     ]).

:- use_module(library(random/temporary)).

:- dynamic situation_module/2.

%!  situation_options(?Situation:compound, ?Options:list) is nondet.
%
%   Sets up Situation using Options.   Establishes the dynamic predicate
%   options for the  temporary  situation   module  used  for persisting
%   situation Now-At and Was-When pairs.
%
%   An important side effect occurs  for   ground  Situation  terms. The
%   implementation creates the situation's temporary  module and applies
%   Options to its new dynamic predicates.  The module(M) option unifies
%   with the newly-created or existing situation module.
%
%   The predicate's determinism collapses to semi-determinism for ground
%   situations.  Otherwise  with  variable   Situation  components,  the
%   predicate  unifies  with  all  matching  situations,  unifying  with
%   module(M) non-deterministically.

situation_options(Situation, Options) :-
    ground(Situation),
    !,
    context_module(M),
    with_mutex(M, situation_options_(Situation, Options)).
situation_options(Situation, Options) :-
    situation_module(Situation, M),
    option(module(M), Options, _).

situation_options_(Situation, Options) :-
    situation_module(Situation, M),
    !,
    option(module(M), Options, _).
situation_options_(Situation, Options) :-
    ground(Situation),
    once(random_temporary_module(M)),
    asserta(situation_module(Situation, M)),
    select_option(module(M), Options, Options_, _),
    dynamic([   M:now/2,
                M:was/2,
                M:currently/2,
                M:previously/2
            ], Options_).

:- multifile canny:property_of_situation/2.

canny:property_of_situation(module(M), Situation) :-
    situation_module(Situation, M).
canny:property_of_situation(defined, Situation) :-
    situation_module(Situation, _).
