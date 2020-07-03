:- module(swi_lists, [ zip/3,
                       indexed_pairs/2,
                       indexed_pairs/3,
                       take_at_most/3
                     ]).

%!  zip(?List1:list, ?List2:list, ?ListOfLists:list(list)) is semidet.
%
%   Zips two lists, List1 and List2, where   each element from the first
%   list pairs with the same element from the second list. Alternatively
%   unzips one list of lists into two lists.
%
%   Only succeeds if the lists and sub-lists have matching lengths.

zip([], [], []).
zip([H1|T1], [H2|T2], [[H1, H2]|T]) :-
    zip(T1, T2, T).

%!  indexed_pairs(?Items:list, ?Pairs:list(pair)) is semidet.
%!  indexed_pairs(?List1:list, ?Index:integer, ?List2:list) is semidet.
%
%   Unifies List1 of items with List2  of   pairs  where  the first pair
%   element is an increasing integer  index.   Index  has some arbitrary
%   starting point, or defaults to 1 for one-based indexing. Unification
%   works in all modes.

indexed_pairs(Items, Pairs) :- indexed_pairs(Items, 1, Pairs).

indexed_pairs([], _, []).
indexed_pairs([H|T0], Index0, [Index0-H|T]) :-
    Index is Index0 + 1,
    indexed_pairs(T0, Index, T).

%!  take_at_most(+Length:integer, +List0, -List) is semidet.
%
%   List takes at most Length elements from   List0.  List for Length of
%   zero is always an empty list, regardless of the incoming List0. List
%   is always empty for an empty   List0, regardless of Length. Finally,
%   elements from List0 unify with  List   until  either Length elements
%   have been seen, or until no more elements at List0 exist.

take_at_most(0, _, []) :- !.
take_at_most(_, [], []) :- !.
take_at_most(N, [H|T0], [H|T]) :- succ(N0, N), take_at_most(N0, T0, T).
