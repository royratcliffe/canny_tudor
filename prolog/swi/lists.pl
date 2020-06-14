:- module(swi_lists, [zip/3, indexed_pairs/2, indexed_pairs/3]).

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
