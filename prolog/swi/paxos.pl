/*  File:    swi/paxos.pl
    Author:  Roy Ratcliffe
    WWW:     https://github.com/royratcliffe
    Created: Jun 12 2021
    Purpose: SWI Paxos

Copyright (c) 2021, Roy Ratcliffe, United Kingdom

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

:- module(swi_paxos,
          [ paxos_quorum_nodes/1,               % -Nodes:list(nonneg)
            paxos_quorum_nth1/1                 % ?Nth1
          ]).
:- autoload(library(lists), [nth1/3]).
:- autoload(library(paxos), [paxos_property/1]).
:- use_module(library(canny/pop)).

%!  paxos_quorum_nodes(-Nodes:list(nonneg)) is semidet.
%
%   Nodes is a list of Paxos consensus nodes who are members of the
%   quorum. Fails if Paxos not yet initialised.

paxos_quorum_nodes(Nodes) :-
    paxos_property(quorum(Quorum)),
    pop_lsbs(Quorum, Nodes).

%!  paxos_quorum_nth1(?Nth1:nonneg) is semidet.
%
%   Unifies Nth1 with the _order_ of this node within the quorum.
%   Answers 1 if this node comes first in the known quorum of
%   consensus nodes, for example.

paxos_quorum_nth1(Nth1) :-
    paxos_property(node(Node)),
    paxos_quorum_nodes(Nodes),
    once(nth1(Nth1, Nodes, Node)).
