/*  File:    cmsis/svd.pl
    Author:  Roy Ratcliffe
    Created: Sep 17 2023
    Purpose: CMSIS SVD
*/

:- module(cmsis_svd,
          [ cmsis_load_svd/2                    % +Spec,+Options
          ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Loads a CMSIS-SVD and optionally asserts its content.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%!  cmsis_load_svd(+Spec, +Options) is semidet.

cmsis_load_svd(Spec, Options) :-
    load_structure(Spec, Content, []),
    content(Content, Options).

content([], _).
content([H|T], Options) :-
    element(H, Options),
    content(T, Options).

element(element(name, _, [Content1]), Options) :-
    option(context([Named1|Named]), Options),
    !,
    arg(1, Named1, Content1),
    named_term([Named1|Named], Term),
    term(Term, Options).
element(element(Tag, _, [Content1]), Options) :-
    option(context(Context), Options),
    !,
    Term1 =.. [Tag, Content1],
    named_term([Term1|Context], Term),
    term(Term, Options).
element(element(Tag, _, Content), Options) :-
    !,
    select_option(context(Context), Options, Options_, []),
    Term =.. [Tag, _],
    content(Content, [context([Term|Context])|Options_]).
element(_, _).

named_term(Context, Term) :-
    include(ground, Context, Named),
    reverse(Named, Named_),
    maplist(term_name_1, Named_, Names),
    maplist(arg(1), Named_, Args),
    atomic_list_concat(Names, '_', Name),
    Term =.. [Name|Args].

term_name_1(Term, Name) :- functor(Term, Name, 1).

term(Term, Options) :-
    (   option(assertz(M), Options)
    ->  assertz(M:Term)
    ;   writeq(Term),
        nl
    ).
