/*  File:    canny/a.pl
    Author:  Roy Ratcliffe
    Created: Jun  7 2021
    Purpose: A* Search

Copyright (c) 2021, Roy Ratcliffe, Northumberland, United Kingdom

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

:- module(canny_a,
          [ a_star/3                    % +Heuristics0,-Heuristics,+Options
          ]).
:- predicate_options(a_star/3, 3,
                     [ initially(any),
                       finally(any),
                       reverse(boolean)
                     ]).
:- autoload(library(option), [option/3, option/2]).
:- autoload(library(lists), [reverse/2]).
:- use_module(library(chr/a_star)).

:- public final/1, expand/3.

%!  a_star(+Heuristics0, -Heuristics, +Options) is det.
%
%   Offers a static non-Constraint Handling Rules interface to a_star/4.
%   Performs a simplified A* search using CHR where the input is a list
%   of _all the possible_ arcs along with their cost. Each element in
%   Heuristics0 is a h/3 term specifying source of the heuristic arc,
%   the arc's destination node and the cost of traversing in-between.
%   Nodes specify distinct but arbitrary terms. Only terms `initial` and
%   `final` have semantic significance. You can override these using
%   Options for `initially` and `finally`. For Options see below.
%
%   Simplifies the CHR implementation by accepting h/3 terms as a list
%   rather than using predicates to expand nodes. We match heuristic
%   terms using member/2 from the list of heuristics. This interface does
%   **not** replace a_star/4 since having a pre-loaded list of
%   heuristics is not always possible or feasible, for example when the
%   number of arcs is very large such as when traversing a grid of arcs.
%
%   Here is a simple example.
%
%       ?- a_star([h(a, b, 1)], A, [initially(a), finally(b)]).
%       A = [h(a, b, 1)].
%
%   Options include:
%
%   - initially(Initial) defines the initial node, defaults to
%     atom `initial`.
%   - finally(Final) defines the final node, atom `final` by default.
%   - reverse(Boolean) reverses the outgoing selected Heuristics so that
%     the order reflects the forward order of traverse. The underlying
%     expansion pushes path nodes to the head of the list resulting in a
%     final-to-initial traversal by default.
%
%   @see https://rosettacode.org/wiki/A*_search_algorithm

a_star(Heuristics0, Heuristics, Options) :-
    option(initially(Initially), Options, initial),
    option(finally(Finally), Options, final),
    a_star(a(Initially, Finally, Heuristics0, []),
           Final^(canny_a:final(Final)),
           Node^Expand^Cost^(canny_a:expand(Node, Expand, Cost)),
           a(_, _, _, Heuristics_)),
    (   option(reverse(true), Options)
    ->  Heuristics_ = Heuristics
    ;   reverse(Heuristics_, Heuristics)
    ).

final(a(Final, Final, _, _)).

expand(a(A, Final, Heuristics0, Heuristics),
       a(B, Final, Heuristics0, [h(A, B, Cost)|Heuristics]),
       Cost) :-
    member(h(A, B, Cost), Heuristics0),
    A \== B.
