/*  File:    cmsis/svd.pl
    Author:  Roy Ratcliffe
    Created: Sep 17 2023
    Purpose: CMSIS SVD
*/

:- module(cmsis_svd,
          [ cmsis_load_svd/2,                   % +Spec,+Options
            cmsis_c99_svd//1
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

    Compiles CMSIS-SVD facts to C99 pre-processor hash-defines.

    The SVD fact base unifies with plain old atoms representing strings
    and numbers, decimal and hexadecimal.

# Example Usage

Suppose that the STMicroelectronics SVD facts have already been loaded
at module `st` using the cmsis_load_svd/2 predicate. The following query
formats C99 `#define` lines for all the STM32H747 M4 core's
peripheral bases.

    forall(
        phrase(cmsis_c99_svd(st:device_peripheral_base('STM32H747_CM4', _, _)), Codes),
        format('~s~n', [Codes])
    ).

#define STM32H747_CM4_COMP1_BASE 0x58003800
#define STM32H747_CM4_CRS_BASE 0x40008400
#define STM32H747_CM4_DAC_BASE 0x40007400
...
#define STM32H747_CM4_RAMECC2_BASE 0x48023000
#define STM32H747_CM4_RAMECC3_BASE 0x58027000
#define STM32H747_CM4_ART_BASE 0x40024400

The order matches the SVD ordering. The following does the same thing
after sorting by the base address ascending.

    A = 'STM32H747_CM4',
    findall(t(A, B, C), st:device_peripheral_baseAddress(A, B, C), D),
    sort(3, @=<, D, E),
    forall(member(t(A, B, C), E),
           (   phrase(cmsis_c99_svd(st:device_peripheral_base(A, B, C)), Codes),
               format('~s~n', [Codes])
           )).

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%!  cmsis_c99_svd(M:Term) is nondet.

cmsis_c99_svd(M:device_peripheral_base(Device, Peripheral, BaseAddress)) -->
    { M:device_peripheral_baseAddress(Device, Peripheral, BaseAddress)
    },
    "#define ",
    atom(Device), "_", atom(Peripheral), "_BASE ",
    atom(BaseAddress).
cmsis_c99_svd(M:device_peripheral_register_offset(Device, Peripheral, Register, D)) -->
    { M:device_peripheral_register_addressOffset(Device, Peripheral, Register, D)
    },
    "#define ",
    atom(Device), "_", atom(Peripheral), "_", atom(Register), "_OFFSET ",
    atom(D).
cmsis_c99_svd(M:device_peripheral_register(Device, Peripheral, Register)) -->
    { M:device_peripheral_baseAddress(Device, Peripheral, _),
      M:device_peripheral_register_addressOffset(Device, Peripheral, Register, _)
    },
    "#define ",
    atom(Device), "_", atom(Peripheral), "_", atom(Register), " (",
    atom(Device), "_", atom(Peripheral), "_BASE + ",
    atom(Device), "_", atom(Peripheral), "_", atom(Register), "_OFFSET)".
cmsis_c99_svd(M:device_peripheral_register_field(Device, Peripheral, Register, Field)) -->
    { M:device_peripheral_register_field_bitOffset(
            Device, Peripheral, Register, Field, Offset),
      M:device_peripheral_register_field_bitWidth(
            Device, Peripheral, Register, Field, Width)
    },
    "#define ",
    atom(Device), "_",
    /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    No need to express the Peripheral if the Register already incorporates
    it. Doing so would unnecessarily double up on the name of the manifest
    constant.
    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
    (   { atom_prefix(Register, Peripheral)
        }
    ->  []
    ;   atom(Peripheral), "_"
    ),
    atom(Register), "_", atom(Field), " (((1 << ",
    atom(Width), ") - 1) << ",
    atom(Offset), ")".

atom(Atom) -->
    { atom_codes(Atom, Codes)
    },
    Codes.
