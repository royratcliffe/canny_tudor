:- module(situation_options, [situation_options/2]).

:- use_module(library(random/temporary)).

:- meta_predicate situation_options(:, ?).

:- predicate_options(situation_options/2, 2,
                     [   module(atom),
                         pass_to((dynamic)/2, 2)
                     ]).

:- multifile canny:property_of_situation/2.

:- dynamic situation_module/2.

canny:property_of_situation(module(M), Situation) :-
    situation_module(Situation, M).

%!  situation_options(?Situation:compound, ?Options:list) is nondet.

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
