/*  File:    canny/redis_streams.pl
    Author:  Roy Ratcliffe
    Created: Sep 24 2022
    Purpose: Canny Redis Streams

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

:- module(redis_streams,
          [ redis_stream_entry/4,               % +Entries,-StreamId,?Tag,-Entry
            redis_stream_entry/5,               % +Reads,-Key,-StreamId,?Tag,-Entry
            xrange/4,                           % +Redis,+Key,-Entries,+Options
            xread/4                             % +Redis,+Streams,-Reads,+Options
          ]).

%!  redis_stream_entry(+Entries:list, -StreamId:pair(nonneg, nonneg),
%!  ?Tag:atom, -Entry:dict) is nondet.
%!  redis_stream_entry(+Reads:list, -Key:atom, -StreamId:pair(nonneg,
%!  nonneg), ?Tag:atom, -Entry:dict) is nondet.
%
%   Unifies non-deterministically with all Entries, or Entry
%   dictionaries embedded with multi-stream Reads. Decodes the stream
%   identifier and the Entry.

redis_stream_entry(Entries, StreamId, Tag, Entry) :-
    member([StreamId0, Entry0], Entries),
    redis_stream_id(StreamId0, StreamId),
    redis_array_dict(Entry0, Tag, Entry).

redis_stream_entry(Reads, Key, StreamId, Tag, Entry) :-
    member([Key, Entries], Reads),
    redis_stream_entry(Entries, StreamId, Tag, Entry).

%!  xrange(+Redis, +Key:atom, -Entries:list, +Options:list) is det.
%
%   Applies range selection to Key stream. Options optionally specify
%   the start and end stream identifiers, defaulting to `-` and `+`
%   respectively or in reverse if `rev(true)` included in Options list.
%   Option `count(Count)` limits the number of entries to read by
%   `Count` items.
%
%   The following always unifies Entries with `[]`.
%
%       xrange(Server, Key, Entries, [start(+)]).
%       xrange(Server, Key, Entries, [rev(true), start(-)]).

xrange(Redis, Key, Entries, Options) :-
    option(rev(Rev), Options, false),
    rev(Rev, XRange, StartDefault, EndDefault),
    option(start(Start), Options, StartDefault),
    option(end(End), Options, EndDefault),
    (   option(count(Count), Options)
    ->  Command =.. [XRange, Key, Start, End, count, Count]
    ;   Command =.. [XRange, Key, Start, End]
    ),
    redis(Redis, Command, Entries).

rev(false, xrange, -, +).
rev(true, xrevrange, +, -).

%!  xread(+Redis, +Streams:dict, -Reads:list, +Options:list) is
%!  semidet.
%
%   Unifies Reads from Streams. Fails on time-out.
%
%   @arg Reads by stream key. The reply has the form [Key, Entries]
%   for each stream where each member of Entries has the form
%   [StreamID, Fields] where Fields is an array of keys and values.

xread(Redis, Streams, Reads, Options) :-
    dict_pairs(Streams, _, Pairs),
    keys_and_stream_ids(Pairs, Keys, StreamIds),
    append(Keys, StreamIds, Arguments___),
    Arguments__ = [streams|Arguments___],
    (   option(block(Block), Options)
    ->  Arguments_ = [block, Block|Arguments__]
    ;   Arguments_ = Arguments__
    ),
    (   option(count(Count), Options)
    ->  Arguments = [count, Count|Arguments_]
    ;   Arguments = Arguments_
    ),
    Command =.. [xread|Arguments],
    redis(Redis, Command, Reads).

keys_and_stream_ids([], [], []).
keys_and_stream_ids([Key-StreamId|T0], [Key|T1], [StreamId|T]) :-
    keys_and_stream_ids(T0, T1, T).
