:- module(swi_ord,
          [ iud/5
          ]).
:- autoload(library(ordsets),
            [ list_to_ord_set/2,
              ord_subtract/3,
              ord_intersect/3
            ]).

%!  iud(+Existing:list,
%!      +Incoming:list,
%!      -Inserts:list,
%!      -Updates:list,
%!      -Deletes:list) is det.
%
%   Take two payload lists, Existing  and   Incoming.  First  convert to
%   element-unique sets. The sets overlap.
%
%       Inserts = Incoming \ Existing
%       Updates = Existing /\ Incoming
%       Deletes = Existing \ Incoming
%
%   Their intersection defines the update set; the set subtraction terms
%   define inserts and deletes respectively.

iud(Existing, Incoming, Inserts, Updates, Deletes) :-
    list_to_ord_set(Existing, Existing_),
    list_to_ord_set(Incoming, Incoming_),
    ord_subtract(Incoming_, Existing_, Inserts),
    ord_subtract(Existing_, Incoming_, Deletes),
    ord_intersect(Existing_, Incoming_, Updates).
