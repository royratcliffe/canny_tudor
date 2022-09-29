/*  File:    canny/redis.pl
    Author:  Roy Ratcliffe
    Created: Sep 24 2022
    Purpose: Canny Redis

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

:- module(canny_redis,
          [ redis_stream_id/2                   % ?StreamId,?RedisTimeSeqPair
          ]).

%!  redis_stream_id(?StreamId, ?RedisTimeSeqPair) is semidet.
%
%   Stream identifier to millisecond and sequence numbers. In
%   practice, the numbers always convert to integers.
%
%   Deliberately validates incoming Redis time and sequence numbers.
%   Both must be integers and both must be zero or more. The predicate
%   fails otherwise. Internally, Redis stores stream identifiers as
%   128-bit unsigned integers split in half for the time and sequence,
%   each of 64 bits.
%
%   @arg StreamId identifies a stream message or entry, element or item.
%   All these terms apply to the contents of a stream, but Redis
%   internally refers to the content as _entries_.
%
%   @arg RedisTimeSeqPair is a pair of non-negative integers, time and
%   sequence. The Redis time equals Unix time multiplied by 1,000; in
%   other words, Unix time in milliseconds.

redis_stream_id(StreamId, RedisTime-Seq) :-
    var(StreamId),
    !,
    integer(RedisTime),
    integer(Seq),
    RedisTime >= 0,
    Seq >= 0,
    atomic_list_concat([RedisTime, Seq], -, StreamId).
redis_stream_id(StreamId, RedisTime-Seq) :-
    split_string(StreamId, -, '', [RedisTime0, Seq0]),
    number_string(RedisTime, RedisTime0),
    number_string(Seq, Seq0).
