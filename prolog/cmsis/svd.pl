/*  File:    cmsis/svd.pl
    Author:  Roy Ratcliffe
    Created: Sep 17 2023
    Purpose: CMSIS SVD
*/

:- module(cmsis_svd,
          [ cmsis_load_svd/2                    % +Spec,+Options
          ]).
:- autoload(library(apply), [include/3, maplist/3]).
:- autoload(library(lists), [reverse/2]).
:- autoload(library(option), [option/2, select_option/4]).
:- autoload(library(sgml), [load_structure/3]).

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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    The SVD fact base unifies with plain old atoms representing strings
    and numbers, decimal and hexadecimal.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

cmsis_c99_svd(M:device_peripheral_base(A, B, C)) -->
    { M:device_peripheral_baseAddress(A, B, C),
      atom_codes(A, A_),
      atom_codes(B, B_),
      atom_codes(C, C_)
    },
    "#define ", A_, "_", B_, "_BASE ", C_, "".
cmsis_c99_svd(M:device_peripheral_register_offset(A, B, C, D)) -->
    { M:device_peripheral_register_addressOffset(A, B, C, D),
      atom_codes(A, A_),
      atom_codes(B, B_),
      atom_codes(C, C_),
      atom_codes(D, D_)
    },
    "#define ", A_, "_", B_, "_", C_, "_OFFSET ", D_, "".
cmsis_c99_svd(M:device_peripheral_register(A, B, C)) -->
    { M:device_peripheral_baseAddress(A, B, _),
      M:device_peripheral_register_addressOffset(A, B, C, _),
      atom_codes(A, A_),
      atom_codes(B, B_),
      atom_codes(C, C_)
    },
    "#define ", A_, "_", B_, "_", C_, " (",
    A_, "_", B_, "_BASE + ",
    A_, "_", B_, "_", C_, "_OFFSET)".
cmsis_c99_svd(M:device_peripheral_register_field(A, B, C, D)) -->
    { M:device_peripheral_register_field_bitOffset(A, B, C, D, Offset),
      M:device_peripheral_register_field_bitWidth(A, B, C, D, Width),
      atom_codes(A, A_),
      /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      No need to express B because C_ incorporates it.

      atom_codes(B, B_),

      - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
      atom_codes(C, C_),
      atom_codes(D, D_),
      atom_codes(Offset, Offset_),
      atom_codes(Width, Width_)
    },
    "#define ", A_, "_", C_, "_", D_, " (((1 << ",
    Width_, ") - 1) << ",
    Offset_, ")".
