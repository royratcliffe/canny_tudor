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
          [ octet_bits/2                % ?Octet:integer,?Fields:list
          ]).
:- use_module(bits).

%!  octet_bits(?Octet:integer, ?Fields:list) is semidet.
%
%   Unifies integral eight-bit Octet with a list of Value:Width terms
%   where the Width integers sum to eight and the Value terms unify with
%   the shifted bit values encoded within the eight-bit byte.
%
%   @arg Octet an eight-bit byte by another name.
%
%   @arg Fields colon-separated value-width terms. The shifted value of
%   the bits comes first before the colon followed by its integer bit
%   width. The list of terms _specify_ an octet by sub-spans of bits, or
%   bit _fields_.

octet_bits(Octet, Fields) :-
    var(Octet),
    !,
    bit_fields(Fields, 8, 0, Octet).
octet_bits(Octet, Fields) :-
    bit_fields(Fields, 8, Octet).
