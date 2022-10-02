/*  File:    canny/hdx.pl
    Author:  Roy Ratcliffe
    Created: Sep 24 2022
    Purpose: Canny Half-Duplex

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

:- module(canny_hdx,
          [ hdx/4,                              % +StreamPair,+Term,-Codes,+TimeOut
            hdx/3                               % +In,-Codes,+TimeOut
          ]).

%!  hdx(+StreamPair, +Term, -Codes, +TimeOut) is semidet.
%!  hdx(+In, -Codes, +TimeOut) is semidet.
%
%   Performs a single half-duplex stream   interaction  with StreamPair.
%   Flushes Term to the output  stream.   Reads  pending  Codes from the
%   input stream within TimeOut  seconds.   Succeeds  when  a write-read
%   cycle completes without timing out; fails on time-out expiry.
%
%   Filling a stream buffer blocks the  calling   thread  if there is no
%   input ready. Pending read operations also block for the same reason.
%   Hence the wait_for_input/3 *must* precede them.
%
%   @arg StreamPair connection from client to server, a
%   closely-associated input and output stream pairing used for
%   half-duplex communication.
%
%   @arg Term to write and flush.
%
%   @arg Codes waited for and extracted from the pending input stream.
%
%   @arg TimeOut in seconds.

hdx(StreamPair, Term, Codes, TimeOut) :-
    stream_pair(StreamPair, In, Out),
    hdx(Out, Term),
    hdx(In, Codes, TimeOut).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

No need to distinguish between command and query at the lower layers of
duplex communication. The upper-level stream determines the mode: either
both halves of the half-duplex cycle: write then read for the query
stream; else write only for the command stream.

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

hdx(Out, Term) :-
    write(Out, Term),
    flush_output(Out).

hdx(In, Codes, TimeOut) :-
    wait_for_input([In], [Ready], TimeOut),
    fill_buffer(Ready),
    read_pending_codes(Ready, Codes, []).
