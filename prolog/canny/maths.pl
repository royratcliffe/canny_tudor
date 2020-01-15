:- module(canny_maths,
          [   remainder/3,
              fmod/3,

              epsilon_equal/2,
              epsilon_equal/3,

              permute_sum/2
          ]).

%!  remainder(+X:number, +Y:number, -Z:number) is det.
%
%   Z is the remainder after dividing X by Y,   calculated  by X - N * Y
%   where N is the nearest integral to X / Y.

remainder(X, Y, Z) :- Z is X - round(X / Y) * Y.

%!  fmod(+X:number, +Y:number, -Z:number) is det.
%
%   Z is the remainder after dividing X by Y, equal to X - N * Y where N
%   is X over Y after truncating its fractional part.

fmod(X, Y, Z) :-
    X_ is abs(X),
    Y_ is abs(Y),
    remainder(X_, Y_, Z_),
    (   sign(Z_) < 0
    ->  Z0 is Z_ + Y_
    ;   Z0 = Z_
    ),
    Z is copysign(Z0, X).

%!  epsilon_equal(+X:number, +Y:number) is semidet.
%!  epsilon_equal(+Epsilons:number, +X:number, +Y:number) is semidet.
%
%   Succeeds only when the absolute  difference   between  the two given
%   numbers X and Y is less than  or   equal  to epsilon, or some factor
%   (Epsilons) of epsilon according to rounding limitations.

epsilon_equal(X, Y) :- epsilon_equal(1, X, Y).

epsilon_equal(Epsilons, X, Y) :- Epsilons * epsilon >= abs(X - Y).

%!  permute_sum(+N:nonneg, -Integers:list(integer)) is nondet.
%
%   Permute sum. Non-deterministically finds all combinations of integer
%   sums between 1 and N. Assumes  that   0<=N.  The  number of possible
%   permutations amounts to 2-to-the-power of  N-1;   for  N=3 there are
%   four as follows: 1+1+1+1, 1+2, 2+1 and 3.

permute_sum(0, []) :- !.
permute_sum(N, [H|T]) :-
    between(1, N, H),
    N0 is N - H,
    permute_sum(N0, T).
