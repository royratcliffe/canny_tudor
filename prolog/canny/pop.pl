:- module(canny_pop,
          [ pop_lsbs/2                           % +A:nonneg,-L:list
          ]).

%!  pop_lsbs(+A:nonneg, -L:list) is det.
%
%   Unifies  non-negative  integer  A   with   its    set   bits   L  in
%   least-significate priority order. Defined only   for non-negative A.
%   Throws a domain error otherwise.
%
%   @error domain_error(not_less_than_one, A) if A less than 0.

pop_lsbs(0, []) :- !.
pop_lsbs(A, [H|T]) :-
    H is lsb(A),
    B is A /\ \(1 << H),
    pop_lsbs(B, T).
