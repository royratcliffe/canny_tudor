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

:- module(canny_redis_streams,
          [ xrange/4,                           % +Redis,+Key,-Entries,+Options
            xread/4,                            % +Redis,+Streams,-Reads,+Options
            xread_call/5,                       % +Redis,+Streams,:Goal,-Fields,+Options
            xread_call/6,                       % +Redis,+Streams,:Goal,?Tag,-Fields,+Options
            xread_ack/6,                        % +Redis,+Key,+Select,+Id,-Fields,+Options
            xadd_group_ack/5                    % +Redis,+KeyGroup,+Add,-Ack,+Options
          ]).
:- autoload(library(option), [option/3, option/2]).
:- autoload(library(lists), [append/3]).
:- autoload(library(redis), [redis/3]).

:- use_module(redis).

:- meta_predicate
    xread_call(+, +, :, -, +),
    xread_call(+, +, :, ?, -, +).

%!  xrange(+Redis, +Key:atom, -Entries:list, +Options:list) is det.
%
%   Applies range selection to Key stream. Options optionally specify
%   the start and end stream identifiers, defaulting to `-` and `+`
%   respectively or in reverse if `rev(true)` included in Options list;
%   the plus stream identifier stands for the maximum identifier, or
%   the newest, whereas the minus identifier stands for the oldest.
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
    ->  Arguments = [count, Count]
    ;   Arguments = []
    ),
    Command =.. [XRange, Key, Start, End|Arguments],
    redis(Redis, Command, Entries).

rev(false, xrange, -, +).
rev(true, xrevrange, +, -).

%!  xread(+Redis, +Streams:dict, -Reads:list, +Options:list) is
%!  semidet.
%
%   Unifies Reads from Streams. Fails on time-out, if option
%   `block(Milliseconds)` specifies a non-zero blocking delay.
%
%   @arg Reads by stream key. The reply has the form Key-Entries
%   for each stream where each member of Entries has the form
%   [StreamID, Fields] where Fields is an array of keys and values.

xread(Redis, Streams, Reads, Options) :-
    redis_keys_and_stream_ids(Streams, _, Keys, StreamIds),
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

%!  xread_call(+Redis, +Streams, :Goal, -Fields, +Options) is semidet.
%!  xread_call(+Redis, +Streams, :Goal, ?Tag, -Fields, +Options) is
%!  semidet.
%
%   Reads Streams continuously until Goal succeeds or times out. Also
%   supports a Redis time limit option so that blocking, if used, does
%   not continue indefinately even on a very busy stream set. The limit
%   applies to any of the given streams; it acts as a time threshold
%   for continuous blocking failures.

xread_call(Redis, Streams, Goal, Fields, Options) :-
    xread(Redis, Streams, Reads, Options),
    redis_last_streams(Reads, _, Streams_),
    xread_call_(Redis, Streams.put(Streams_), Goal, Reads, Fields, Options).

xread_call_(_Redis, _Streams, Goal, Reads, Fields, Options) :-
    redis_stream_read(Reads, Key, StreamId, Fields),
    call(Goal, Key, StreamId, Fields),
    select_option(key(Key), Options, _, _),
    select_option(id(StreamId), Options, _, _),
    !.
xread_call_(Redis, Streams, Goal, _Reads, Fields, Options) :-
    streams_options(Streams, Options),
    xread_call(Redis, Streams, Goal, Fields, Options).

xread_call(Redis, Streams, Goal, Tag, Fields, Options) :-
    xread(Redis, Streams, Reads, Options),
    redis_last_streams(Reads, _, Streams_),
    xread_call_(Redis, Streams.put(Streams_), Goal, Reads, Tag, Fields, Options).

xread_call_(_Redis, _Streams, Goal, Reads, Tag, Fields, Options) :-
    redis_stream_read(Reads, Key, StreamId, Tag, Fields),
    call(Goal, Key, StreamId, Tag, Fields),
    select_option(key(Key), Options, _, _),
    select_option(id(StreamId), Options, _, _),
    !.
xread_call_(Redis, Streams, Goal, _Reads, Tag, Fields, Options) :-
    streams_options(Streams, Options),
    xread_call(Redis, Streams, Goal, Tag, Fields, Options).

streams_options(Streams, Options) :-
    (   option(threshold(Threshold), Options)
    ->  !,
        dict_pairs(Streams, _, Pairs),
        maplist(stream_redis_time, Pairs, RedisTimes),
        max_list(RedisTimes, RedisTime),
        RedisTime < Threshold
    ;   true
    ).

stream_redis_time(_Key-StreamId, RedisTime) :-
    redis_stream_id(StreamId, RedisTime, _Seq).

%!  xread_ack(+Redis, +Key, +Select, +Id, -Fields, +Options) is semidet.
%
%   Unifies Fields with entry from Key with fields matching Select and
%   with a stream identifier starting at Id.
%
%   @arg Redis connection to Redis server.
%
%   @arg Key stream from which to read.
%
%   @arg Select dictionary to match.
%
%   @arg Id identifying start of window. The millisecond Redis time
%   marks the start of the reading window from which to scan. The end
%   extends forward in time by the limiting delay if Options defines a
%   limit(Milliseconds) option in milliseconds.
%
%   @arg Fields unified with matching entry.
%
%   @arg Options includes limit/1 for limiting Redis time in
%   milliseconds relative to the millisecond time of the Id matched
%   stream identifier.

xread_ack(Redis, Key, Select, Id, Fields, Options) :-
    redis_stream_id(Id, RedisTime0, Seq0),
    (   option(limit(Limit), Options)
    ->  Threshold is RedisTime0 + Limit,
        Options_ = [threshold(Threshold)|Options]
    ;   Options_ = Options
    ),
    xread_call(Redis, _{}.put(Key, RedisTime0-0),
               xread_ack_(Select, RedisTime0-Seq0), _, Fields,
               [ id(RedisTime-Seq)|Options_
               ]),
    select_option(id(RedisTime-Seq), Options, _, _),
    (   option(delay(Delay), Options)
    ->  Delay is RedisTime - RedisTime0
    ;   true
    ).

:- public xread_ack_/6.

xread_ack_(Select, Id, _Key, _StreamId, _Tag, Fields) :-
    Select :< Fields,
    redis_stream_id(Fields.get(id), Id).

%!  xadd_group_ack(+Redis, +KeyGroup, +Add, -Ack, +Options) is semidet.
%
%   Adds a request Add to some Key:Group stream which Group consumers
%   read and add an Ack to Key stream on completion, success or failure.
%   The protocol makes a simple assumption: that the Ack entry carries a
%   `group` field matching Group.
%
%   @arg Redis connection to server.
%
%   @arg KeyGroup is the colon-separated Key and Group.
%
%   @arg Add dictionary of fields to add.
%
%   @arg Ack dictionary of acknowledged fields, including `group`
%   matching the Group.
%
%   @arg Options for reading acknowledgement.

xadd_group_ack(Redis, Key:Group, Add, Ack, Options) :-
    xadd(Redis, Key:Group, Id, Add),
    xread_ack(Redis, Key, _{group:Group}, Id, Ack, Options).
