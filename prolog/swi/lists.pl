:- module(swi_lists, [zip/3]).

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
