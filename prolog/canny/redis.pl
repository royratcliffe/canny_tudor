/*  File:    canny/redis.pl
    Author:  Roy Ratcliffe
    Created: Sep 24 2022
    Purpose: Canny Redis

Copyright (c) 2022, 2024, Roy Ratcliffe, Northumberland, United Kingdom

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
            redis_date_time/3,                  % +RedisTime,-DateTime,+TimeZone
            redis_stream/3,                     % +Redis, --Stream, +DoConnect
            redis_stream/2,                     % +Redis, --Stream
            redis_write_msg/2,                  % +Redis, +Msg
            redis_write_msg/3,                  % +Redis, +Msg, +DoFlush
            redis_read_msg/2                    % +Redis, -Msg
          ]).
:- autoload(library(lists), [member/2]).
:- autoload(library(redis), [redis_array_dict/3]).
:- autoload(library(apply), [maplist/3]).

:- use_module(library(canny/bytes)).

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
%   Allows for "byte" reads, as follows, where the resulting values unify
%   with lists of byte codes.
%
%       xread(default, _{key: 0}, Reads as bytes, []).
%
%   @arg Reads is a list of [Key, Entries] lists, a list of lists. The
%   sub-lists always have two items: the Key of the stream followed by
%   another sub-list of stream entries.

redis_stream_read(Reads, Key, StreamId, Fields) :-
    key_entries(Key, Entries, Reads),
    redis_stream_entry(Entries, StreamId, Fields).

redis_stream_read(Reads, Key, StreamId, Tag, Fields) :-
    key_entries(Key, Entries, Reads),
    redis_stream_entry(Entries, StreamId, Tag, Fields).

key_entries(Key, Entries, Reads) :-
    member([Key0, Entries], Reads),
    atom_string(Key, Key0).

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

                /*******************************
                *       M e s s a g e s        *
                *******************************/

%!  redis_stream(+Redis, --Stream, +DoConnect) is det.
%!  redis_stream(+Redis, --Stream) is det.
%
%   Connects a Redis specification to a connected stream. Breaks the
%   Redis package encapsulation by accessing the redis:redis_stream/3
%   private predicate.
%
%   The arity-2 version automatically reconnects.

redis_stream(Redis, Stream, DoConnect) :-
    redis:redis_stream(Redis, Stream, DoConnect).

redis_stream(Redis, Stream) :- redis_stream(Redis, Stream, true).

%!  redis_write_msg(+Redis, +Msg, +DoFlush) is semidet.
%!  redis_write_msg(+Redis, +Msg) is semidet.
%
%   Writes a Redis message *without* reading a reply. Atoms become
%   upper-case strings. Lists become arrays.

redis_write_msg(Redis, Msg) :- redis_write_msg(Redis, Msg, true).

redis_write_msg(Redis, Msg, DoFlush) :-
    redis_stream(Redis, Stream),
    write_msg(Msg, Stream),
    (   DoFlush == true
    ->  flush_output(Stream)
    ;   true
    ).

write_msg(str(nul), Stream) :- !, put_bytes(Stream, `$-1\r\n`).
write_msg(str(Str), Stream) :- !,
    length(Str, Len),
    format(Stream, '$~d\r\n', [Len]),
    put_bytes(Stream, Str),
    put_bytes(Stream, `\r\n`).
write_msg(arr(Arr), Stream) :- !,
    length(Arr, Len),
    format(Stream, '*~d\r\n', [Len]),
    write_arr(Arr, Stream).
write_msg(String, Stream) :- string(String), !,
    string_bytes(String, Bytes, utf8),
    write_msg(str(Bytes), Stream).
write_msg(Atom, Stream) :- atom(Atom), !,
    string_upper(Atom, String),
    write_msg(String, Stream).
write_msg(List, Stream) :- is_list(List),
    write_msg(arr(List), Stream).

write_arr([], _Stream) :- !.
write_arr([Msg|Msgs], Stream) :-
    write_msg(Msg, Stream),
    write_arr(Msgs, Stream).

%!  redis_read_msg(+Redis, -Msg) is semidet.
%
%   Reads a Redis message at Msg. Presumes UTF-8 encoding for simple
%   errors and simple strings but does not make the same assumption for
%   bulk strings. The latter remain unencoded bytes. Operates
%   recursively for arrays of messages.
%
%   String message terms str(Str) have three forms: (1) where the
%   argument Str is `nul`; (2) where the argument is a simple UTF-8
%   string; or (3) where the argument is a list of bulk bytes.

redis_read_msg(Redis, Msg) :-
    redis_stream(Redis, Stream),
    read_msg(Stream, Msg).

read_msg(Stream, Msg) :-
    get_byte(Stream, Byte),
    read_msg(Byte, Stream, Msg).

read_msg(0'-, Stream, err(Str)) :- read_utf8_line(Stream, Str).
read_msg(0'+, Stream, str(Str)) :- read_utf8_line(Stream, Str).
read_msg(0':, Stream, int(Int)) :- read_int_line(Stream, Int).
read_msg(0'$, Stream, str(Str)) :-
    read_int_line(Stream, Len),
    read_str(Len, Stream, Str).
read_msg(0'*, Stream, arr(Arr)) :-
    read_int_line(Stream, Len),
    read_arr(Len, Stream, Arr).

read_utf8_line(Stream, UTF8) :-
    read_line_to_bytes(Stream, Bytes),
    string_bytes(UTF8, Bytes, utf8).

read_num_line(Stream, Num) :-
    read_utf8_line(Stream, Str),
    atom_number(Str, Num).

read_int_line(Stream, Int) :-
    read_num_line(Stream, Int),
    integer(Int).

%!  read_str(+Len, +Stream, -Str) is semidet.
%
%   Reads a bulk string of Len bytes *without* decoding, Lua style.
%   Strings are zero or more unencoded bytes or `nul`. This allows for
%   disparate decodings and encodings when written to Redis.

read_str(-1, _Stream, nul) :- !.
read_str(Len, Stream, Str) :-
    read_bytes(Len, Stream, Str),
    read_line_to_bytes(Stream, []).

read_bytes(0, _Stream, []) :- !.
read_bytes(Len, Stream, [Byte|Bytes]) :-
    get_byte(Stream, Byte),
    succ(Len0, Len),
    read_bytes(Len0, Stream, Bytes).

%!  read_arr(+Len, +Stream, -Arr) is semidet.
%
%   Reads an array of messages recursively.

read_arr(0, _Stream, []) :- !.
read_arr(Len, Stream, [Msg|Msgs]) :-
    read_msg(Stream, Msg),
    succ(Len0, Len),
    read_arr(Len0, Stream, Msgs).

%!  read_line_to_bytes(+Stream, -Bytes) is semidet.
%
%   Does not use get_code/2. Fails if no line found in Stream. The line
%   must terminate with carriage return *and* line feed. Fails if
%   the line contains a new line before the carriage return, or if the
%   end-of-file occurs before the line terminates. Answers the line's
%   Bytes *without* the line terminators. Assumes that the stream has
%   binary type and octet encoding.

read_line_to_bytes(Stream, Bytes) :-
    get_byte(Stream, Byte),
    read_line_to_bytes_(Byte, Stream, Bytes).

read_line_to_bytes_(-1, _Stream, _Bytes) :- !, fail.
read_line_to_bytes_(0'\n, _Stream, _Bytes) :- !, fail.
read_line_to_bytes_(0'\r, Stream, []) :- !, get_byte(Stream, 0'\n).
read_line_to_bytes_(Byte, Stream, [Byte|Bytes]) :-
    get_byte(Stream, Byte_),
    read_line_to_bytes_(Byte_, Stream, Bytes).
