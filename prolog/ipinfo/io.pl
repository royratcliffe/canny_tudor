/*  File:    ipinfo_io.pl
    Author:  Roy Ratcliffe
    Created: Oct 30 2021
    Purpose: ipinfo.io

Copyright (c) 2021, Roy Ratcliffe, United Kingdom

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

:- module(ipinfo_io,
          [ ipinfo_io/1                         % -Dict:dict
          ]).
:- autoload(library(http/http_client), [http_get/3]).

:- ensure_loaded(library(http/http_json)).

%!  ipinfo_io(-Dict:dict) is det.
%
%   IP information using the ipinfo.io site.
%   ```
%   ?- ipinfo_io(A), print_term(A, []).
%   _{ city:"Sunderland",
%      country:"GB",
%      hostname:"XXX.XX.XXX.XX.threembb.co.uk",
%      ip:"XXX.XX.XXX.XX",
%      loc:"54.9046,-1.3822",
%      org:"AS206067 Hutchison 3G UK Limited",
%      postal:"SR1",
%      readme:"https://ipinfo.io/missingauth",
%      region:"England",
%      timezone:"Europe/London"
%    }
%   ```

ipinfo_io(Dict) :-
    http_get('http://ipinfo.io/json', Dict, [json_object(dict)]).
