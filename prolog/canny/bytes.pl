/*  File:    canny/bytes.pl
    Author:  Roy Ratcliffe
    Created: Jun  1 2024
    Purpose: Canny Bytes

Copyright (c) 2024, Roy Ratcliffe, Northumberland, United Kingdom

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

:- module(canny_bytes,
          [ put_bytes/2                         % +Stream, +Bytes:list
          ]).

%!  put_bytes(+Stream, +Bytes:list) is det.
%
%   Puts zero or more Bytes to a Stream.
%
%   A good reason exists for _putting bytes_ rather than writing codes.
%   The put_byte/1 predicate throws with permission error when writing
%   to a text stream. Bytes are *not* Unicode text; they have an
%   entirely different ontology.
%
%   @see Character representation manual section
%   at https://www.swi-prolog.org/pldoc/man?section=chars for more
%   details about the difference between codes, characters and bytes.

put_bytes(Stream, Bytes) :- put_bytes_(Bytes, Stream).

put_bytes_([], _Stream).
put_bytes_([Byte|Bytes], Stream) :-
    put_byte(Stream, Byte),
    put_bytes_(Bytes, Stream).
