:- module(canny_random_names,
          [   random_name/2             % -LHS, -RHS
          ]).

%!  random_name(-LHS:atom, -RHS:atom) is det.
%
%   Unifies LHS-RHS with one random name, a randomised selection from
%   all possible names.
%
%   Note, this does *not* work in (+, ?) or (?, +) or (+, +) modes, even
%   if required. Predicate random_member/2 fails semi-deterministically
%   if the given atom fails to match the randomised selection.

random_name(LHS, RHS) :-
    random_name_(LHS0, lhs(LHS0), LHS),
    random_name_(RHS0, rhs(RHS0), RHS).

random_name_(Template, Goal, Member) :-
    findall(Template, Goal, Members),
    random_member(Member, Members).

:- public
    lhs/1,
    rhs/1.

lhs(a).
lhs(b).
lhs(c).

rhs(x).
rhs(y).
rhs(z).
