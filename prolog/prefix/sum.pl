/*  File:    prefix/sum.pl
    Author:  Roy Ratcliffe
    Created: Dec 20 2025
    Purpose: Module for computing prefix sums and range sums.

Copyright (c) 2025, Roy Ratcliffe, Northumberland, United Kingdom

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

:- module(prefix_sum,
          [ prefix_sum/2,                       % +Numbers:list(number), -PrefixSum:list(number)
            range_sum/4                         % +PrefixSum:list(number), +Index0:integer, +Index:integer, -Sum:number
          ]).
:- autoload(library(lists), [nth0/3]).

/** <module> Prefix Sums
 * This module provides functionality to compute the prefix sum of an
 * array and to calculate the sum of elements in a specified range using
 * the prefix sum array. It is useful for efficiently answering
 * range-sum queries after a preprocessing step. The prefix sum at each
 * index is the sum of all previous elements including the current one.
 * It allows for quick calculation of sums over subarrays and is a
 * common technique in algorithm design.
 *
 * @author Roy Ratcliffe
 */

%!  prefix_sum(+Numbers:list(number), -PrefixSum:list(number)) is det.
%
%   Calculates the inclusive prefix sum of   the given array. The prefix
%   sum at each index is the sum  of all previous elements including the
%   current one. This is  useful   for  efficiently computing cumulative
%   sums. The function is deterministic and will always produce the same
%   output for the same input.
%
%   The Numbers array must be  ground   numerical  values. The resulting
%   PrefixSum array will have  the  same   length  as  the input Numbers
%   array.
%
%   Why is this useful?
%
%       - It allows for quick calculation of sums over subarrays.
%
%       - It can be used in various algorithms that require cumulative
%         sums.
%
%   It answers range-sum queries in constant   time  O(1) after a linear
%   time  O(n)  preprocessing  step.  This  is  particularly  useful  in
%   scenarios where multiple range-sum queries are performed on the same
%   array. The sum  of  A[i..j]  can   be  computed  as  PrefixSum[j]  -
%   PrefixSum[i-1].
%
%   @arg Numbers A list of numerical values  for which the prefix sum is
%   to be calculated.
%
%   @arg PrefixSum The resulting array containing the prefix sums.

prefix_sum([], []).
prefix_sum([H|T0], [H|T]) :- prefix_sum(T0, H, T).

prefix_sum([], _, []).
prefix_sum([H0|T0], Prefix, [H|T]) :- H is H0 + Prefix, prefix_sum(T0, H, T).

%!  range_sum(+PrefixSum:list(number), +Index0:integer, +Index:integer, -Sum:number) is det.
%
%   Computes the sum of elements in the original array from Index0 to
%   Index using the provided PrefixSum array.
%
%   This function assumes that the PrefixSum array has been computed
%   using the prefix_sum/2 predicate.
%
%   @arg PrefixSum The prefix sum array.
%   @arg Index0 The starting index of the range (inclusive).
%   @arg Index The ending index of the range (inclusive).
%   @arg Sum The resulting sum of elements from Index0 to Index.

range_sum(PrefixSum, Index0, Index, Sum) :-
    (   succ(Index_, Index0)
    ->  nth0(Index_, PrefixSum, Left)
    ;   Left = 0
    ),
    nth0(Index, PrefixSum, Right),
    Sum is Right - Left.
