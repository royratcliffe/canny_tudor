/*  File:    canny/shifter.pl
    Author:  Roy Ratcliffe
    Created: Sep 16 2023
    Purpose: Bit Shifter

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

:- module(canny_shifter,
          [ bit_shift/3                         % +Shifter,?Left,?Right
          ]).

%!  bit_shift(+Shifter, ?Left, ?Right) is semidet.
%
%   Shifts bits left or right depending on the argument mode. Mode (+,
%   -, +) shifts left whereas mode (+, +, -) shifts right. The first
%   argument specifies the position of the bit or bits in Left, the
%   second argument, while the third argument specifies the aligned
%   Right bits.
%
%   The Shifter argument provides three different ways to specify a bit
%   shift and bit width: either by an exclusive range using `+` and `-`
%   terms; or an _inclusive_ range using `:` terms; or finally just a
%   single bit shift which implies a width of one bit. Colons operate
%   inclusively whereas plus and minus apply exclusive upper ranges.
%
%   It first finds the amount of Shift required and the bit Width. After
%   computing the lefthand and righthand bit masks, it finally performs
%   a shift-mask or mask-shift for left and right shifts respectively.
%
%   @arg Shifter is a Shift+Width, Shift-Width, High:Low, Low:High or
%   just a single integer Shift for single bits.
%
%   @arg Left is the left-shifted integer.
%
%   @arg Right is the right-shifted integer.

bit_shift(Shifter, Left, Right) :-
    shifter(Shifter, Shift, Width),
    RightMask is (1 << Width) - 1,
    LeftMask is RightMask << Shift,
    shift(Left, Right, Shift, LeftMask).

shifter(Shift+Width, Shift, Width) :- !.
shifter(Shift0-Width, Shift, Width) :- !, Shift is Shift0 - Width.
shifter(Low:High, Low, Width) :- Low =< High, !, Width is High - Low + 1.
shifter(High:Low, Low, Width) :- !, Width is High - Low + 1.
shifter(Shift, Shift, 1).

shift(Left, Right, Shift, LeftMask), var(Left) =>
    Left is (Right << Shift) /\ LeftMask.
shift(Left, Right, Shift, LeftMask) =>
    Right is (Left /\ LeftMask) >> Shift.
