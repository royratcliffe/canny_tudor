/*  File:    canny/roman.pl
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

/** <module> Roman Numerals

Fundamentally, the Roman "number" represents a numerical sum using 9, 5,
4 and 1 as the principal factors.   `I=1`,  `V=5` and `X=10`. Prefix `V`
and `X` with `I` to  represent  `4`   and  `9`,  respectively.  The same
pattern recurs for `L=50`, `C=100`, `D=500` and `M=1000`.

In [definite-clause grammar](https://www.metalevel.at/prolog/dcg)  terms
this formula becomes the following logical phrase.

```prolog
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
```

Here, I ignore the upper-lower-case  issue.   Roman  numerals have upper
case only, although frequently the numerals can have either case, albeit
consistently all upper or all lower.

These are the factors. Composite numerals   comprise a sequence of these
sub-phrases. They need  to  add  up   to  some  numerical  value.  Enter
constraint-logic  programming  for  finite  domains    or   CLP(FD)  for
symbolically representing logical relations between numbers.

```prolog
roman_numerals(Number) -->
    { Number #> 0,
      Number #= Number0 + Number1
    },
    roman_numeral(Number0),
    roman_numerals(Number1).
roman_numerals(0) --> [].
```

This is a recursive  phrase.  For  every   `Number>0`,  there  is  a sum
`Number=Number0+Number1` where `Number0`  is  a   Roman  numeral,  *and*
`Number1` is the sum of  subsequent   Roman  numerals. Finally, no Roman
numeral `[]` corresponds to `0`. The  initial clause applies [arithmetic
constraints](https://www.swi-prolog.org/man/clpfd.html).  The  terms  do
not need to bind to integers   initially.  Hence, the constraints appear
first at the head of the predicate.

"Quod erat demonstrandum!"

### How does it work?

Prolog searches the problem space using   the given constraints. The sum
of numbers must always sum to a positive   result.  The sum can never be
zero or below. Romans  did  not   grasp  zero  or negatives, apparently.
Perhaps they did  grasp  the  abstractions   but  found  in  them little
practical value; Romans were pragmatic people after all.

The backtracking logic has an interesting  side effect. It finds **all**
the possible Roman representations of a   number. Using the simple Roman
combinatorial logic, one number has multiple alternative representations
in Roman numerals.

Take `5` for example.

```prolog
?- phrase(roman_numerals(5), A), string_codes(B, A).
A = [86],
B = "V" ;
A = `IVI`,
B = "IVI" ;
A = `IIV`,
B = "IIV" ;
A = `IIIII`,
B = "IIIII".
```

There are four alternative representations:

    1. `V`
    2. `IV+I=V`
    3. `I+IV=V`
    4. `I+I+I+I+I=V`

Clause order matters for the   `roman_numeral//1` predicate; big numbers
must come first so that the solution finds larger factors before smaller
ones. Romans being Roman, the "correct"   representation is the shortest
possible representation, or put another way:   the form with the largest
possible factors. Hence big factors take priority over smaller ones.

It helps, therefore, to  wrap  the  grammar   using  a  cut  (`!`).  The
following terminates the search for a solution   when it finds the first
one; the first solution being the sum with the largest factors.

```prolog
roman_number(Roman, Number) :-
    phrase(roman_numerals(Number), Roman),
    !.
```

What's Roman for `9999`?

```prolog
?- roman_number(A, 9999).
A = `MMMMMMMMMCMXCIX`.

?- roman_number(`MMMMMMMMMCMXCIX`, B).
B = 9999.
```

The Roman term, `A` in  the  previous   query,  unifies  with  a list of
character codes hence the backticks.

## Strengths and Weaknesses

The logical implementation has  a   somewhat  surprising  outcome: Roman
numerals have alternative renderings.

The representation for 0 is **logically** blank. That makes total sense,
come to think about it. Subtly,  the   unification  does  not *fail*. It
succeeds with nothing instead.  This  implies   that  the  Romans  could
represent zero by writing nothing.

```prolog
?- phrase(roman_numerals(0), A).
A = [].
```

Prolog makes the implementation  pretty  simple,   but  there  are  some
subtleties:  clause  order  has  semantic  significance;  green  cutting
likewise.

*/

%!  roman_number(?Roman:codes, ?Number:integer) is semidet.
%
%   Wrap the Roman numerals grammar using a cut. The predicate concludes
%   the search for a solution upon finding   the first, which is the sum
%   with the largest factors.
%
%  @arg Roman  A list of character codes representing the Roman numeral.
%  @arg Number An integer corresponding to the Roman numeral.

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
%
%  The base case handles the situation where   Number is 0, resulting in
%  an empty list.
%
%  @arg Number An integer corresponding to the Roman numeral.

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
