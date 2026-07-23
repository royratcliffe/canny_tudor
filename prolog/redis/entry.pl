:- module(redis_entry, [redis_entry_id/3]).

%!  redis_entry_id(?Stamp, ?Seq, ?Id) is semidet.
%
%   Relates a Redis stream  entry  ID   to  its  constituent  parts: the
%   timestamp (Stamp) and the sequence number  (Seq). The ID is expected
%   to be in the format "Stamp-Seq".
%
%   @arg Stamp The timestamp part of the Redis stream entry ID.
%
%   @arg Seq The sequence number part of the Redis stream entry ID.
%
%   @arg Id The Redis stream entry ID in the format "Stamp-Seq". Renders
%   as an atom, which is  the   standard  representation of Redis stream
%   entry IDs, but accepts a string as well.  The atom is used for Redis
%   commands, while the string is used for JSON serialization.

redis_entry_id(Stamp, Seq, Id), var(Id) =>
    atom_number(Stamp0, Stamp),
    atom_number(Seq0, Seq),
    atomic_list_concat([Stamp0, Seq0], -, Id).
redis_entry_id(Stamp, Seq, Id) =>
    atomic_list_concat([Stamp0, Seq0], -, Id),
    atom_number(Stamp0, Stamp),
    atom_number(Seq0, Seq).
