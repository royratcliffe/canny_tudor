:- module(pop,
          [ poplsbs/2                           % +A:nonneg,-L:list
          ]).
:- use_module(library(plunit), [begin_tests/1, end_tests/1]).

%!  poplsbs(+A:nonneg, -L:list) is det.
%
%   Unifies  non-negative  integer  A   with   its    set   bits   L  in
%   least-significate priority order. Defined only   for non-negative A.
%   Throws a domain error otherwise.
%
%   @error domain_error(not_less_than_one, A) if A less than 0.

poplsbs(0, []) :- !.
poplsbs(A, [H|T]) :-
    H is lsb(A),
    B is A /\ \(1 << H),
    poplsbs(B, T).

:- begin_tests(pop).

test(poplsbs, [true(A==[0])]) :- poplsbs(1, A).
test(poplsbs, [true(A==[0, 2])]) :- poplsbs(5, A).
test(poplsbs, [true(A==[1, 2])]) :- poplsbs(6, A).
test(poplsbs, [true(A==[0, 1, 2, 3])]) :- poplsbs(15, A).
test(poplsbs, [true(A==[0, 1, 3])]) :- poplsbs(15-4, A).
test(poplsbs, [true(A==[2, 5, 6])]) :- poplsbs(100, A).
test(poplsbs, [throws(error(domain_error(not_less_than_one, -1), _))]) :-
    poplsbs(-1, _).

:- end_tests(pop).
