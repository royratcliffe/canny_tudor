/*  File:    canny/crc.pl
    Author:  Roy Ratcliffe
    Created: Aug  6 2023
    Purpose: CRC

Copyright (c) 2023, Roy Ratcliffe, Northumberland, United Kingdom

Permission is hereby granted, free of charge,  to any person obtaining a
copy  of  this  software  and    associated   documentation  files  (the
"Software"), to deal in  the   Software  without  restriction, including
without limitation the rights to  use,   copy,  modify,  merge, publish,
distribute, sublicense, and/or sell  copies  of   the  Software,  and to
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

:- module(canny_crc,
          [ crc/2,                              % +Predefined,-Check
            crc_property/2,                     % +Check,?Property
            crc/3                               % +Check0,+Term,-Check
          ]).
:- autoload(library(apply), [foldl/4]).
:- autoload(library(option), [option/2]).

:- use_module(bits, [rbit/3]).

%!  crc(+Predefined, -Check) is semidet.

crc(crc-8, crc(16'107, 16'0, [])).
crc(crc-16-mcrf4xx, crc(16'11021, 16'FFFF, [reverse])).

%!  crc_property(+Check, ?Property) is semidet.

crc_property(crc(Poly, _Check, _Options), poly(Poly)).
crc_property(crc(_Poly, Check, _Options), check(Check)).

%!  crc(+Check0, +Term, -Check) is semidet.

crc(crc(Poly, Check0, Options), Byte, crc(Poly, Check, Options)) :-
    integer(Byte),
    !,
    0 =< Byte,
    Byte < 256,
    poly_deg(Poly, Deg),
    (   option(reverse, Options)
    ->  check_right(Deg, Poly, Check0, Byte, Check_)
    ;   check_left(Deg, Poly, Check0, Byte, Check_)
    ),
    Check is Check_ /\ ((1 << Deg) - 1).
crc(Check0, List, Check) :-
    is_list(List),
    foldl(crc_, List, Check0, Check).

crc_(Term, Check0, Check) :- crc(Check0, Term, Check).

:- table check_left/3.

check_left(Poly, Check0, Check) :- check_left(8, Poly, Check0, Check).

check_left(0, _Poly, Check, Check) :- !.
check_left(Count, Poly, Check0, Check) :-
    succ(Count_, Count),
    poly_deg(Poly, Deg),
    bit_left(Deg, Check0, Bit, Check1),
    xor(Bit, Check1, Poly, Check_),
    check_left(Count_, Poly, Check_, Check).

check_left(8, Poly, Check0, Byte, Check) :-
    Byte_ is Byte xor Check0,
    check_left(Poly, Byte_, Check).
check_left(16, Poly, Check0, Byte, Check) :-
    Byte_ is Byte xor (Check0 >> 8),
    check_left(Poly, Byte_, Check_),
    Check is Check_ xor ((Check0 << 8) /\ 16'FF00).
check_left(24, Poly, Check0, Byte, Check) :-
    Byte_ is Byte xor (Check0 >> 16),
    check_left(Poly, Byte_, Check_),
    Check is Check_ xor ((Check0 << 8) /\ 16'FF_FF00).
check_left(32, Poly, Check0, Byte, Check) :-
    Byte_ is Byte xor (Check0 >> 24),
    check_left(Poly, Byte_, Check_),
    Check is Check_ xor ((Check0 << 8) /\ 16'FFFF_FF00).
check_left(64, Poly, Check0, Byte, Check) :-
    Byte_ is Byte xor (Check0 >> 56),
    check_left(Poly, Byte_, Check_),
    Check is Check_ xor ((Check0 << 8) /\ 16'FFFF_FFFF_FFFF_FF00).

:- table check_right/3.

check_right(Poly, Check0, Check) :-
    poly_deg(Poly, Deg),
    rbit(Deg, Poly, Poly_),
    check_right(8, Poly_, Check0, Check).

check_right(0, _Poly, Check, Check) :- !.
check_right(Count, Poly, Check0, Check) :-
    succ(Count_, Count),
    bit_right(Check0, Bit, Check1),
    xor(Bit, Check1, Poly, Check_),
    check_right(Count_, Poly, Check_, Check).

check_right(8, Poly, Check0, Byte, Check) :-
    Byte_ is Byte xor Check0,
    check_right(Poly, Byte_, Check).
check_right(16, Poly, Check0, Byte, Check) :-
    Byte_ is Byte xor (Check0 /\ 16'00FF),
    check_right(Poly, Byte_, Check_),
    Check is Check_ xor (Check0 >> 8).
check_right(24, Poly, Check0, Byte, Check) :-
    Byte_ is Byte xor (Check0 /\ 16'00_00FF),
    check_right(Poly, Byte_, Check_),
    Check is Check_ xor (Check0 >> 8).
check_right(32, Poly, Check0, Byte, Check) :-
    Byte_ is Byte xor (Check0 /\ 16'0000_00FF),
    check_right(Poly, Byte_, Check_),
    Check is Check_ xor (Check0 >> 8).
check_right(64, Poly, Check0, Byte, Check) :-
    Byte_ is Byte xor (Check0 /\ 16'0000_0000_0000_00FF),
    check_right(Poly, Byte_, Check_),
    Check is Check_ xor (Check0 >> 8).

bit_left(Deg, Byte0, Bit, Byte) :-
    Bit is getbit(Byte0, Deg - 1),
    Byte is (Byte0 << 1) /\ ((1 << Deg) - 1).

bit_right(Byte0, Bit, Byte) :-
    Bit is getbit(Byte0, 0),
    Byte is Byte0 >> 1.

xor(0, Byte, _Poly, Byte).
xor(1, Byte0, Poly, Byte) :- Byte is Byte0 xor Poly.

:- table poly_deg/2.

poly_deg(Poly, Deg) :- deg(Deg), poly_deg_(Poly, Deg), !.

deg(8).
deg(16).
deg(24).
deg(32).
deg(64).

poly_deg_(Poly, Deg) :-
    Low is 1 << Deg,
    Low =< Poly,
    High is Low << 1,
    Poly < High.
