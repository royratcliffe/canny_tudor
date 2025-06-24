/*  File:    canny/url.pl
    Author:  Roy Ratcliffe
    Created: Jul  9 2024
    Purpose: URLs

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

:- module(canny_url,
          [ url_encoded//1,
            url_coded/2                         % ?Decoded, ?Encoded
          ]).
:- autoload(library(dcg/high_order), [sequence/4]).

%!  url_encoded(?Code)// is nondet.
%
%   Similar to uri_encoded/3 but simpler.
%
%   URL encodes a Code. Uses the C-symbol code type (including letters,
%   digits and underscores) conservatively to determine whether or not
%   to encode to percentage-prefixed hexadecimal. Liberally decodes by
%   accepting any code except the percentage code. Percents must decode
%   with a pair of hexadecimal digits; decoding otherwise fails.

url_encoded(Code) -->
    { nonvar(Code)
    },
    encoded(Code).
url_encoded(Code) -->
    decoded(Code).

encoded(Code) -->
    { code_type(Code, ascii), code_type(Code, csym)
    },
    !, [Code].
encoded(Code) -->
    { X1 is Code >> 4,
      X2 is Code /\ 16'f,
      code_type(Code1, xdigit(X1)),
      code_type(Code2, xdigit(X2))
    },
    "%", [Code1, Code2].

decoded(Code) -->
    "%", !, [Code1, Code2],
    { code_type(Code1, xdigit(X1)),
      code_type(Code2, xdigit(X2)),
      Code is (X1 << 4) \/ X2
    }.
decoded(Code) --> [Code].

%!  url_coded(?Decoded, ?Encoded) is semidet.
%
%   Encodes or decodes a sequence of codes.

url_coded(Decoded, Encoded) :-
    once(phrase(sequence(url_encoded, Decoded), Encoded)).
