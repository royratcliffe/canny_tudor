:- begin_tests(ieee_754).

:- use_module('754').

frld(A, B) :-
    ieee_754:frexp(A, Y, Exp),
    ieee_754:ldexp(Y, B, Exp).

frld(A) :- frld(A, A).

test(frexp_ldexp) :- frld(1.1).
test(frexp_ldexp) :- frld(123.123).

test(frexp, [true(A-B==1.5NaN- -1)]) :- ieee_754:frexp(1.0Inf, A, B).

:- end_tests(ieee_754).
