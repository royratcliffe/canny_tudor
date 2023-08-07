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
          []).

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
