:- module(ieee_754, []).

frexp(X, Y, Exp) :- float_parts(X, Y, 2, Exp).

ldexp(X, Y, Exp) :- Y is X * 2 ** Exp.
