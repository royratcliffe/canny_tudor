/*  File:    roman.pl
    Author:  Roy Ratcliffe
    Created: Jan 18 2025
    Purpose: Roman Numerals

Copyright (c) 2025, Roy Ratcliffe, Northumberland, United Kingdom

Permission is hereby granted, free of charge,  to any person obtaining a
copy  of  this  software  and    associated   documentation  files  (the
"Software"), to deal in  the   Software  without  restriction, including
without limitation the rights to  use,   copy,  modify,  merge, publish,
distribute, sub-license, and/or sell copies  of   the  Software,  and to
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

:- module(canny_roman,
          [ roman_number/2,
            roman_numerals//1
          ]).
:- use_module(library(clpfd)).

%!  roman_number(?Roman:codes, ?Number:integer) is semidet.
%
%   Wrap the Roman numerals grammar using a cut. The predicate concludes
%   the search for a solution upon finding   the first, which is the sum
%   with the largest factors.

roman_number(Roman, Number) :-
    phrase(roman_numerals(Number), Roman),
    !.

%!  roman_numerals(?Number:integer)// is nondet.
%
%   Tail-recursively unifies with a  Roman   numeral  phrase.  For every
%   number greater than 0, there  is  a   sum  where  Number = Number0 +
%   Number1. In this equation, Number0 represents a Roman numeral, while
%   Number1 denotes the sum of   subsequent Roman numerals. Importantly,
%   no Roman numeral corresponds to 0.   The  initial clause establishes
%   arithmetic constraints.

roman_numerals(Number) -->
    { Number #> 0,
      Number #= Number0 + Number1
    },
    roman_numeral(Number0),
    roman_numerals(Number1).
roman_numerals(0) --> [].

roman_numeral(1000) --> "M".
roman_numeral(900)  --> "CM".
roman_numeral(500)  --> "D".
roman_numeral(400)  --> "CD".
roman_numeral(100)  --> "C".
roman_numeral(90)   --> "XC".
roman_numeral(50)   --> "L".
roman_numeral(40)   --> "XL".
roman_numeral(10)   --> "X".
roman_numeral(9)    --> "IX".
roman_numeral(5)    --> "V".
roman_numeral(4)    --> "IV".
roman_numeral(1)    --> "I".
