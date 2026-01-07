/*  File:    swi/lists.pl
    Author:  Roy Ratcliffe
    Created: Aug 20 2019
    Purpose: Lists for SWI-Prolog

Copyright (c) 2019-2026, Roy Ratcliffe, Northumberland, United Kingdom

Permission is hereby granted, free of charge,  to any person obtaining a
copy  of  this  software  and    associated   documentation  files  (the
"Software"), to deal in  the   Software  without  restriction, including
without limitation the rights to  use,   copy,  modify,  merge, publish,
distribute, sub-license, and/or sell copies  of   the  Software,  and to
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

:- module(swi_lists, [ zip/3,
                       pairs/2,         % ?Items, ?Pairs
                       indexed/2,       % ?Items, ?Pairs
                       indexed/3,       % ?List1, ?Index, ?List2
                       take_at_most/3,  % +Length, +List0, -List
                       select1/3,       % +Indices, +List0, -List
                       select_apply1/3, % +Indices, :Goal, +Extra
                       comb2/2          % ?List1, ?List2
                     ]).
:- autoload(library(apply), [maplist/3]).
:- autoload(library(lists), [nth1/3]).

:- meta_predicate select_apply1(+, :, +).

/** <module> Extensions for SWI-Prolog Lists
 *
 * This module provides various list   manipulation predicates not found
 * in the standard library. Each predicate works in multiple modes where
 * possible.
 *
 * @version 2026-02-10
 * @author Roy Ratcliffe
 */

%!  zip(?List1:list, ?List2:list, ?ListOfLists:list(list)) is semidet.
%
%   Zips two lists, List1 and List2, where   each element from the first
%   list pairs with the same element from the second list. Alternatively
%   unzips one list of lists into two lists.
%
%   Only succeeds if the lists and sub-lists have matching lengths.

zip([], [], []).
zip([H1|T1], [H2|T2], [[H1, H2]|T]) :- zip(T1, T2, T).

%!  pairs(?Items:list, ?Pairs:list(pair)) is semidet.
%
%   Pairs up list elements, or unpairs them   in  (-, +) mode. Pairs are
%   First-Second terms where First  and   Second  match  two consecutive
%   Items. Unifies a list with its paired list.
%
%   There needs to be an even number  of list elements. This requirement
%   proceeds from the definition of pairing;   it  pairs the entire list
%   including the last. The predicate fails otherwise.
%
%   @arg Items A list of interleaved First, Second pairs
%   @arg Pairs A list of First-Second pairs

pairs([], []).
pairs([H1, H2|T0], [H1-H2|T]) :- pairs(T0, T).

%!  indexed(?Items:list, ?Pairs:list(pair)) is semidet.
%!  indexed(?List1:list, ?Index:integer, ?List2:list) is semidet.
%
%   Unifies List1 of items with List2  of   pairs  where  the first pair
%   element is an increasing integer  index.   Index  has some arbitrary
%   starting point, or defaults to 1 for one-based indexing. Unification
%   works in all modes.

indexed(Items, Pairs) :- indexed(Items, 1, Pairs).

indexed([], _, []).
indexed([H|T0], Index0, [Index0-H|T]) :-
    Index is Index0 + 1,
    indexed(T0, Index, T).

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

%!  select1(+Indices, +List0, -List) is det.
%
%   Selects List elements by index from   List0.  Applies nth1/3 to each
%   element of Indices. The 1  suffix   of  the predicate name indicates
%   one-based Indices used for selection.   Mirrors select/3 except that
%   the predicate picks elements from a  list   by  index rather than by
%   element removal.
%
%   @see nth1/3
%   @see select/3

select1(Indices, List0, List) :- maplist(select1_(List0), Indices, List).

select1_(List, Index, Elem) :- nth1(Index, List, Elem).

%!  select_apply1(+Indices, :Goal, +Extra) is nondet.
%
%   Selects one-based index  arguments  from   Extra  and  applies these
%   extras to Goal.
%
%   @see apply/2

select_apply1(Indices, Goal, Extra) :-
    select1(Indices, Extra, Extra1),
    apply(Goal, Extra1).

%!  comb2(?List1, ?List2) is nondet.
%
%   Unifies List2 with all combinations of   List1.  The length of List2
%   defines the number of elements in  List1   to  take  at one time. It
%   follows that length of List1 must  not   be  less  than List2. Fails
%   otherwise.
%
%   @see http://kti.ms.mff.cuni.cz/~bartak/prolog/combinatorics.html

comb2([H|T0], [H|T]) :- comb2(T0, T).
comb2([_|T0], [H|T]) :- comb2(T0, [H|T]).
comb2(_, []).
