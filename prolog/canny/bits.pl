/*  File:    canny/bits.pl
    Author:  Roy Ratcliffe
    Created: Aug 15 2022
    Purpose: Canny Bits

Copyright (c) 2022, Roy Ratcliffe, Northumberland, United Kingdom

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

:- module(canny_bits,
          [ bits/5,
            bits/4,
            bits/3,
            bit_fields/3,                       % +Fields,+Shift,+Int
            bit_fields/4                        % +Fields,+Shift,+Int0,-Int
          ]).

:- use_module(library(clpfd)).

%!  bits(+Shift, +Width, ?Word, ?Bits, ?Rest) is semidet.
%
%   Unifies Bits within a Word using Shift  and Width. All arguments are
%   integers treated as words of arbitrary bit-width.
%
%   The implementation uses relational integer arithmetic, i.e. CLP(FD).
%   Hence allows for forward and backward   transformations from Word to
%   Bits and vice versa. Integer Word applies a Shift and bit Width mask
%   to integer Bits. Bits is always   a  smaller integer. Decomposes the
%   problem  into  shifting  and  masking.    Treats   these  operations
%   separately.
%
%   @arg Width of Bits from  Word  after   Shift.  Width  of zero always
%   fails.

bits(Shift, Width, Word, Bits, Rest) :-
    Mask #= (1 << Width - 1) << Shift,
    Bits0 #= Word /\ Mask,
    Rest #= Word xor Bits0,
    Word #= Bits0 /\ Mask \/ Rest,
    Bits #= Bits0 // (1 << Shift),
    Bits0 #= Bits << Shift.

bits(Shift-Width, Word, Bits, Rest) :- bits(Shift, Width, Word, Bits, Rest).

bits(Shift-Width, Word, Bits) :- bits(Shift, Width, Word, Bits, _Rest).

%!  bit_fields(+Fields:list, +Shift:integer, +Int:integer) is semidet.
%!  bit_fields(+Fields:list, +Shift:integer, +Int0:integer,
%!  -Int:integer) is semidet.
%
%   Two versions of the predicate unify `Value`:`Width` bit fields with
%   integers. The arity-3 version requires a bound Int from which to
%   find unbound values in the Fields.
%
%   The predicates are semi-deterministic. They can fail. Failure occurs
%   when the bit-field Width integers do *not* sum to Shift.

bit_fields([], 0, _Int).
bit_fields([Value:Width|Rest], Shift, Int) :-
    Shift_ is Shift - Width,
    Value is (Int >> Shift_) /\ ((1 << Width) - 1),
    bit_fields(Rest, Shift_, Int).

bit_fields([], 0, Int, Int).
bit_fields([Value:Width|Rest], Shift, Int0, Int) :-
    Shift_ is Shift - Width,
    Int_ is Int0 \/ ((Value /\ ((1 << Width) - 1)) << Shift_),
    bit_fields(Rest, Shift_, Int_, Int).
