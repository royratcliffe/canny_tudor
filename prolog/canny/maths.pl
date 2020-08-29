:- module(canny_maths,
          [   remainder/3,
              fmod/3,

              epsilon_equal/2,
              epsilon_equal/3,

              frexp/3,
              ldexp/3
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

%!  frexp(+X:number, -Y:number, -Exp:integer) is det.
%
%   Answers mantissa Y and exponent Exp for floating-point number X.
%
%   @arg Y is the floating-point mantissa   falling  within the interval
%   [0.5, 1.0). Note the non-inclusive upper bound.

frexp(X, Y, Exp) :- float_parts(X, Y, 2, Exp).

%!  ldexp(+X:number, -Y:number, +Exp:integer) is det.
%
%   Loads exponent. Multiplies X by 2 to  the power Exp giving Y. Mimics
%   the C math ldexp(x, exp) function.
%
%   Uses an unusual argument order. Ordering aligns   X,  Y and Exp with
%   frexp/3. Uses ** rather than ^ operator. Exp is an integer.
%
%   @arg X is some floating-point value.
%   @arg Y is X times 2 to the power Exp.
%   @arg Exp is the exponent, typically an integer.

ldexp(X, Y, Exp) :- Y is X * 2 ** Exp.
