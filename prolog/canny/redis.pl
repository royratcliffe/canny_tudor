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
          [ redis_last_streams/2,               % +Reads,-Streams:list
            redis_last_streams/3,               % +Reads,?Tag,-Streams:dict
            redis_last_stream_entry/3,          % +Streams,-StreamId,-Fields
            redis_last_stream_entry/4,          % +Streams,-StreamId,?Tag,-Fields
            redis_keys_and_stream_ids/4,        % +Streams,?Tag,-Keys,-StreamIds
            redis_keys_and_stream_ids/3,        % +Pairs,-Keys,-StreamIds,
            redis_stream_read/4,                % +Reads,-Key,-StreamId,-Fields
            redis_stream_read/5,                % +Reads,-Key,-StreamId,?Tag,-Fields
            redis_stream_entry/3,               % +Entries,-StreamId,-Fields
            redis_stream_entry/4,               % +Entries,-StreamId,?Tag,-Fields
            redis_stream_id/1,                  % ?RedisTimeSeqPair
            redis_stream_id/2,                  % ?StreamId,?RedisTimeSeqPair
            redis_stream_id/3,                  % ?StreamId,?RedisTime,?Seq
            redis_time/1,                       % +RedisTime
            redis_date_time/3                   % +RedisTime,-DateTime,+TimeZone
          ]).
:- autoload(library(lists), [member/2]).
:- autoload(library(redis), [redis_array_dict/3]).
:- autoload(library(apply), [maplist/3]).

                /*******************************
                *       S t r e a m s          *
                *******************************/

%!  redis_last_streams(+Reads, -Streams:list) is det.
%!  redis_last_streams(+Reads, ?Tag, -Streams:dict) is det.
%
%   Collates the last Streams for a given list of Reads, the reply from
%   an XREAD command. The implementation assumes that each stream's read
%   reply has one entry at least, else the stream does not present a
%   reply.

redis_last_streams(Reads, Streams) :-
    maplist(redis_last_stream, Reads, Streams).

redis_last_stream([Key, Entries], Key-StreamId) :-
    redis_last_stream_entry(Entries, StreamId, _).

redis_last_streams(Reads, Tag, Streams) :-
    redis_last_streams(Reads, Streams0),
    dict_create(Streams, Tag, Streams0).

%!  redis_last_stream_entry(+Entries, -StreamId, -Fields) is semidet.
%!  redis_last_stream_entry(+Entries:list(list), -StreamId:atom,
%!  ?Tag:atom, -Fields:dict) is semidet.
%
%   Unifies with the last StreamId and Fields. It fails for empty
%   Entries. Each entry comprises a StreamId and a set of Fields.

redis_last_stream_entry([[StreamId, Fields]], StreamId, Fields) :-
    !.
redis_last_stream_entry([_|Entries], StreamId, Fields) :-
    redis_last_stream_entry(Entries, StreamId, Fields).

redis_last_stream_entry(Entries, StreamId, Tag, Fields) :-
    redis_last_stream_entry(Entries, StreamId, Fields0),
    redis_array_dict(Fields0, Tag, Fields).

%!  redis_keys_and_stream_ids(+Streams, ?Tag, -Keys, -StreamIds) is det.
%!  redis_keys_and_stream_ids(+Pairs, -Keys, -StreamIds) is det.
%
%   Streams or Pairs of Keys and StreamIds. Arity-3 exists with Tag in
%   order to unify with a dictionary by Tag.
%
%   @arg Streams is a dictionary of stream identifiers, indexed by
%   stream key.
%
%   @arg Keys is a list of stream keys.
%
%   @arg StreamIds is a list of corrected stream identifiers. The
%   predicate applies redis_stream_id/3 to the incoming identifiers,
%   allowing for arbitrary milliseconds-sequence pairs including
%   implied missing zero sequence number.

redis_keys_and_stream_ids(Streams, Tag, Keys, StreamIds) :-
    dict_pairs(Streams, Tag, Pairs),
    redis_keys_and_stream_ids(Pairs, Keys, StreamIds).

redis_keys_and_stream_ids([], [], []).
redis_keys_and_stream_ids([Key-StreamId0|T0], [Key|T1], [RedisTime-Seq|T]) :-
    redis_stream_id(StreamId0, RedisTime, Seq),
    redis_keys_and_stream_ids(T0, T1, T).

%!  redis_stream_read(+Reads, -Key, -StreamId, -Fields) is nondet.
%!  redis_stream_read(+Reads, -Key, -StreamId, ?Tag, -Fields) is nondet.
%
%   Unifies with all Key, StreamId and array of Fields for all Reads.
%
%   @arg Reads is a list of [Key, Entries] lists, a list of lists. The
%   sub-lists always have two items: the Key of the stream followed by
%   another sub-list of stream entries.

redis_stream_read(Reads, Key, StreamId, Fields) :-
    member([Key, Entries], Reads),
    redis_stream_entry(Entries, StreamId, Fields).

redis_stream_read(Reads, Key, StreamId, Tag, Fields) :-
    member([Key, Entries], Reads),
    redis_stream_entry(Entries, StreamId, Tag, Fields).

%!  redis_stream_entry(+Entries, -StreamId, -Fields) is nondet.
%!  redis_stream_entry(+Entries:list, -StreamId:pair(nonneg, nonneg),
%!  ?Tag:atom, -Fields:dict) is nondet.
%!  redis_stream_entry(+Reads:list, -Key:atom, -StreamId:pair(nonneg,
%!  nonneg), ?Tag:atom, -Fields:dict) is nondet.
%
%   Unifies non-deterministically with all Entries, or Fields
%   dictionaries embedded with multi-stream Reads. Decodes the stream
%   identifier and the Entry.
%
%   @arg Entries is a list of [StreamId, Fields] lists, another list of
%   lists. Each sub-list describes an "entry" within the stream, a
%   pairing between an identifier and some fields.

redis_stream_entry(Entries, StreamId, Fields) :-
    member([StreamId0, Fields], Entries),
    redis_stream_id(StreamId0, StreamId).

redis_stream_entry(Entries, StreamId, Tag, Fields) :-
    redis_stream_entry(Entries, StreamId, Fields0),
    redis_array_dict(Fields0, Tag, Fields).

%!  redis_stream_id(?RedisTimeSeqPair) is semidet.
%!  redis_stream_id(?StreamId:text, ?RedisTimeSeqPair) is semidet.
%!  redis_stream_id(?StreamId:text, ?RedisTime:nonneg, ?Seq:nonneg) is
%!  semidet.
%
%   Stream identifier to millisecond and sequence numbers. In
%   practice, the numbers always convert to integers.
%
%   Deliberately validates incoming Redis time and sequence numbers.
%   Both must be integers and both must be zero or more. The predicates
%   fail otherwise. Internally, Redis stores stream identifiers as
%   128-bit unsigned integers split in half for the time and sequence
%   values, each of 64 bits.
%
%   The 3-arity version of the predicate handles extraction of time and
%   sequence integers from arbitrary stream identifiers: text or
%   compound terms, including implied zero-sequence stream identifier
%   with a single non-negative integer representing a millisecond Unix
%   time.
%
%   @arg StreamId identifies a stream message or entry, element or item.
%   All these terms apply to the contents of a stream, but Redis
%   internally refers to the content as _entries_.
%
%   @arg RedisTimeSeqPair is a pair of non-negative integers, time and
%   sequence. The Redis time equals Unix time multiplied by 1,000; in
%   other words, Unix time in milliseconds.

redis_stream_id(RedisTime-Seq) :-
    redis_time(RedisTime),
    integer(Seq),
    Seq >= 0.

redis_stream_id(StreamId, RedisTime-Seq) :-
    var(StreamId),
    !,
    redis_stream_id(RedisTime-Seq),
    atomic_list_concat([RedisTime, Seq], -, StreamId).
redis_stream_id(StreamId, RedisTime-Seq) :-
    (   atom(StreamId)
    ->  true
    ;   string(StreamId)
    ),
    split_string(StreamId, -, '', [RedisTime0, Seq0]),
    number_string(RedisTime, RedisTime0),
    number_string(Seq, Seq0),
    redis_stream_id(RedisTime-Seq).

redis_stream_id(RedisTime-Seq, RedisTime, Seq) :-
    redis_stream_id(RedisTime-Seq),
    !.
redis_stream_id(RedisTime, RedisTime, 0) :-
    redis_time(RedisTime),
    !.
redis_stream_id(StreamId, RedisTime, Seq) :-
    redis_stream_id(StreamId, RedisTime-Seq).

%!  redis_time(+RedisTime) is semidet.
%
%   Successful when RedisTime is a positive integer. Redis times amount
%   to millisecond-scale Unix times.
%
%   @arg RedisTime in milliseconds since 1970.

redis_time(RedisTime) :-
    integer(RedisTime),
    RedisTime >= 0.

%!  redis_date_time(+RedisTime, -DateTime, +TimeZone) is det.
%
%   Converts RedisTime to DateTime within TimeZone.

redis_date_time(RedisTime, DateTime, TimeZone) :-
    Stamp is RedisTime / 1000,
    stamp_date_time(Stamp, DateTime, TimeZone).
