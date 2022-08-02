:- module(canny_arity, [arities/2]).

%!  arities(?Arities0:compound, ?Arities:list) is semidet.
%
%   Suppose that you want to accept arity arguments of the form {A, ...}
%   where A is the first integer element   of  a comma-separated list of
%   arity numbers. The Arities0 form is  a compound term enclosed within
%   braces, comprising integers  delimited  by   commas.  The  arities/2
%   predicate extracts the arities as a list.
%
%   Empty lists fail. Also, lists containing non-integers fail to unify.
%   The implementation works forwards and   backwards: arity compound to
%   arity list or vice versa, mode (+, -) or mode (-, +).

arities({Arity}, [Arity]) :- integer(Arity), !.
arities(Arities0, Arities) :-
    Arities0 =.. [{}, Arities_],
    arities_(Arities_, Arities).

arities_((Arity0, Arity), [Arity0, Arity]) :-
    integer(Arity0),
    integer(Arity),
    !.
arities_(Arities0, [H|T]) :-
    Arities0 =.. [',', H, T0],
    integer(H),
    arities_(T0, T).
