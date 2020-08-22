:- module(os_lc, [lc_r/1, lc_r/2, lc_r/3, lc/3]).

:- use_module(library(print/table)).

%!  lc_r(+Extensions:list) is det.
%
%   Recursively counts and prints a table of  the number of lines within
%   read-access files having one of the   given  Extensions found in the
%   current directory or one of its  sub-directories. Prints the results
%   in line-count descending order with the  total count appearing first
%   against an asterisk, standing for all lines counted.

lc_r(Extensions) :-
    lc_r(Pairs0, [extensions(Extensions)]),
    sort(2, @>=, Pairs0, Pairs),
    maplist(arg(2), Pairs, Counts),
    sum_list(Counts, Sum),
    print_table(member(_-_, [(*)-Sum|Pairs])).

%!  lc_r(-Pairs, +Options) is det.
%
%   Counts lines in files recursively within the current directory.

lc_r(Pairs, Options) :- lc_r(., Pairs, Options).

%!  lc_r(+Directory, -Pairs, +Options) is det.
%
%   Counts lines within files starting at Directory.

lc_r(Directory, Pairs, Options) :-
    lc(Directory, Pairs, [recursive(true)|Options]).

%!  lc(+Directory, -Pairs, +Options) is det.
%
%   Counts lines in files  starting  at   Directory  and  using Options.
%   Counts  for  each  file  concurrently  in  order  to  maintain  high
%   performance.
%
%   @arg Pairs is a list of atom-integer   pairs where the relative path
%   of a matching text file is the first pair-element, and the number of
%   lines counted is the second pair-element.

lc(Directory, Pairs, Options) :-
    findall(Member0, directory_member(Directory, Member0,
                                      [ access(read)|Options
                                      ]), Members),
    concurrent_maplist(member_pair, Members, Pairs).

:- public member_pair/2.

member_pair(Member, Member-Count) :-
    setup_call_cleanup(
        open(Member, read, Stream),
        stream_count(Stream, Count),
        close(Stream)).

%!  stream_count(+Stream, -Count:integer) is det.
%
%   Counts lines in Stream. Works by   reading  and counting lines until
%   end of stream. Count becomes the line  count for the remaining codes
%   within Stream. Only gives  a  complete   count  if  Stream initially
%   remains unread. The count excludes any last empty line.

stream_count(Stream, Count) :-
    read_line_to_codes(Stream, Codes),
    stream_count_(Codes, Stream, 0, Count).

stream_count_(end_of_file, _, Count, Count) :- !.
stream_count_(_, Stream, Count0, Count) :-
    Count_ is Count0 + 1,
    read_line_to_codes(Stream, Codes),
    stream_count_(Codes, Stream, Count_, Count).
