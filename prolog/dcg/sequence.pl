/*  File:    dcg/sequence.pl
    Author:  Roy Ratcliffe
    Created: Aug 22 2026
    Purpose: DCG Sequence with Flexible Separator Handling

Copyright (c) 2026, Roy Ratcliffe, Northumberland, United Kingdom

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

:- module(dcg_sequence,
          [ sequence_as//3 % +List, :OnElement, :OnSeparator
          ]).

:- meta_predicate sequence_as(+, :, :, ?, ?).

%!  sequence_as(+List, :OnElement, :OnSeparator)// is semidet.
%
%   Matches or generates a sequence of elements from List, applying
%   OnElement to each element and OnSeparator between elements.
%
%   This DCG rule is useful for parsing or generating sequences of elements
%   where the separator is not fixed. It solves a problem with the standard
%   `sequence//3` predicate, which cuts when it encounters any separator,
%   including a final separator that does not have a subsequent element. This
%   variation looks for a separator with a subsequent element but ignores a
%   separator without a subsequent element that may be present at the end of the
%   sequence, allowing for more flexible parsing and generation of sequences.
%
%   Fails for trailing separators without a subsequent element. A
%   separator requires something to follow it, and if nothing follows,
%   the sequence fails. This behaviour is useful for parsing sequences
%   where a trailing separator is not allowed.
%
%   @arg List        The list of elements to match or generate.
%   @arg OnElement   The DCG rule to apply to each element.
%   @arg OnSeparator The DCG rule to apply between elements.

sequence_as([H|T], OnElement, OnSeparator) -->
    call(OnElement, H),
    (   OnSeparator,
        sequence_as(T, OnElement, OnSeparator)
    ->  !
    ;   { T = []
        }
    ).
