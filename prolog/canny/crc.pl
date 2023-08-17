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
          [ crc/2,                              % +Predefined,-CRC
            crc_property/2,                     % +CRC,?Property
            crc/3,                              % +CRC0,+Term,-CRC
            crc_16_mcrf4xx/1,                   % -Check
            crc_16_mcrf4xx/3                    % +Check0,+Data,-Check
          ]).
:- autoload(library(apply), [foldl/4]).
:- autoload(library(option), [option/2]).

:- use_module(bits, [rbit/3]).

%!  crc(+Predefined, -CRC) is semidet.
%
%   Builds a predefined CRC accumulator.
%
%   @arg Predefined specifies a predefined CRC computation.
%
%   @arg CRC a newly-initialised CRC term with the correct polynomial,
%   initial value and any necessary options such as bit reversal and
%   inversion value.

crc(crc-8,          crc(16'107, 16'0, [])).
crc(crc-8-itu,      crc(16'107, 16'55, [xor(16'55)])).
crc(crc-16-mcrf4xx, crc(16'1_1021, 16'FFFF, [reverse])).
crc(crc-x25,        crc(16'1_1021, 16'0000, [reverse, xor(16'FFFF)])).
crc(crc-32,         crc(16'1_04C1_1DB7, 16'0000_0000, [reverse, xor(16'FFFF_FFFF)])).
crc(crc-32-bzip2,   crc(16'1_04C1_1DB7, 16'0000_0000, [xor(16'FFFF_FFFF)])).
crc(crc-64-jones,   crc(16'1_AD93_D235_94C9_35A9, 16'FFFF_FFFF_FFFF_FFFF, [reverse])).

%!  crc_property(+CRC, ?Property) is semidet.
%
%   Extracts the CRC's checksum for comparison, or unifies with other
%   interesting values belonging to a CRC accumulator.

crc_property(crc(Poly, _Check, _Options), poly(Poly)).
crc_property(crc(_Poly, Check, _Options), check(Check)).

%!  crc(+CRC0, +Term, -CRC) is semidet.
%
%   Mutates CRC0 to CRC by feeding in a byte code, or a list of
%   byte codes.
%
%   @arg CRC0 the initial or thus-far accumulated CRC.
%   @arg Term a byte code or a list of byte codes.
%   @arg CRC the updated CRC.

crc(crc(Poly, Check0, Options), Byte, crc(Poly, Check, Options)) :-
    integer(Byte),
    !,
    0 =< Byte,
    Byte < 256,
    poly_deg(Poly, Deg),
    xor(Check0, Check1, Options),
    (   option(reverse, Options)
    ->  check_right(Deg, Poly, Check1, Byte, Check_)
    ;   check_left(Deg, Poly, Check1, Byte, Check_)
    ),
    Check2 is Check_ /\ ((1 << Deg) - 1),
    xor(Check2, Check, Options).
crc(Check0, List, Check) :-
    is_list(List),
    foldl(crc_, List, Check0, Check).

crc_(Term, Check0, Check) :- crc(Check0, Term, Check).

:- table check_left/3.

check_left(Poly, Check0, Check) :-
    poly_deg(Poly, Deg),
    Check1 is Check0 << (Deg - 8),
    check_left(8, Poly, Check1, Check_),
    Check is Check_ /\ ((1 << Deg) - 1).

check_left(0, _Poly, Check, Check) :- !.
check_left(Count, Poly, Check0, Check) :-
    succ(Count_, Count),
    poly_deg(Poly, Deg),
    bit_left(Deg, Check0, Bit, Check1),
    xor(Bit, Check1, Poly, Check_),
    check_left(Count_, Poly, Check_, Check).

check_left(Deg, Poly, Check0, Byte, Check) :-
    Shift is Deg - 8,
    Byte_ is Byte xor (Check0 >> Shift),
    check_left(Poly, Byte_, Check_),
    CheckMask is (1 << Shift) - 1,
    Check is Check_ xor ((Check0 /\ CheckMask) << 8).

:- table check_right/3.

check_right(Poly, Check0, Check) :-
    poly_deg(Poly, Deg),
    rbit(Deg, Poly, Poly_),
    check_right(8, Poly_, Check0, Check_),
    Check is Check_ /\ ((1 << Deg) - 1).

check_right(0, _Poly, Check, Check) :- !.
check_right(Count, Poly, Check0, Check) :-
    succ(Count_, Count),
    bit_right(Check0, Bit, Check1),
    xor(Bit, Check1, Poly, Check_),
    check_right(Count_, Poly, Check_, Check).

check_right(_Deg, Poly, Check0, Byte, Check) :-
    Byte_ is Byte xor (Check0 /\ 16'FF),
    check_right(Poly, Byte_, Check_),
    Check is Check_ xor (Check0 >> 8).

bit_left(Deg, Int0, Bit, Int) :-
    Bit is getbit(Int0, Deg - 1),
    Int is (Int0 << 1) /\ ((1 << Deg) - 1).

bit_right(Int0, Bit, Int) :-
    Bit is getbit(Int0, 0),
    Int is Int0 >> 1.

xor(0, Int, _Poly, Int).
xor(1, Int0, Poly, Int) :- Int is Int0 xor Poly.

xor(Check0, Check, Options) :-
    (   option(xor(Check_), Options)
    ->  Check is Check_ xor Check0
    ;   Check = Check0
    ).

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

%!  crc_16_mcrf4xx(-Check) is det.
%
%   Initialises CRC-16/MCRF4XX checksum.

crc_16_mcrf4xx(16'FFFF).

%!  crc_16_mcrf4xx(+Check0, +Data, -Check) is det.
%
%   Accumulates CRC-16/MCRF4XX checksum using optimal shifting and
%   exclusive-OR operations.

crc_16_mcrf4xx(Check0, Data, Check) :-
    integer(Data),
    !,
    Data_ is (Check0 /\ 16'FF) xor (Data /\ 16'FF),
    Data__ is Data_ xor ((Data_ << 4) /\ 16'FF),
    Check is (Check0 >> 8) xor (Data__ << 8) xor (Data__ << 3) xor (Data__ >> 4).
crc_16_mcrf4xx(Check0, Data, Check) :-
    is_list(Data),
    foldl(crc_16_mcrf4xx_, Data, Check0, Check).

crc_16_mcrf4xx_(Data, Check0, Check) :- crc_16_mcrf4xx(Check0, Data, Check).
