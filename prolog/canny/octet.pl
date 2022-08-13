/*  File:    canny/octet.pl
    Author:  Roy Ratcliffe
    Created: Aug 13 2022
    Purpose: Canny Octet

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

:- module(canny_octet,
          [ octet_bits/2 % ?Octet:integer,?ValuesAndWidths:list
          ]).

%!  octet_bits(?Octet:integer, ?ValuesAndWidths:list) is semidet.
%
%   Unifies integral eight-bit Octet with a list of Value:Width terms
%   where the Width integers sum to eight and the Value terms unify with
%   the shifted bit values encoded within the eight-bit byte.

octet_bits(Octet, ValuesAndWidths) :-
    var(Octet),
    !,
    bits(ValuesAndWidths, 8, 0, Octet).
octet_bits(Octet, ValuesAndWidths) :-
    bits(ValuesAndWidths, 8, Octet).

bits([], 0, _Octet).
bits([Value:Width|T], Shift, Octet) :-
    Shift_ is Shift - Width,
    Value is (Octet >> Shift_) /\ ((1 << Width) - 1),
    bits(T, Shift_, Octet).

bits([], 0, Octet, Octet).
bits([Value:Width|T], Shift, Octet0, Octet) :-
    Shift_ is Shift - Width,
    Octet_ is Octet0 \/ ((Value /\ ((1 << Width) - 1)) << Shift_),
    bits(T, Shift_, Octet_, Octet).
