:- begin_tests(ieee_754).

:- use_module('754').

:- use_module(library(canny/maths)).

pack_unpack(Width, Float0, Float) :-
    frexp(Float0, Sig0, Exp0),
    ieee_754:pack(Width, Word, Sig0, Exp0),
    ieee_754:pack(Width, Word, Sig, Exp),
    ldexp(Sig, Float, Exp).

test(pack_unpack) :- pack_unpack(32, 3.0, A), epsilon_equal(3.0, A).
test(pack_unpack) :- pack_unpack(32, -3.0, A), epsilon_equal(-3.0, A).

:- end_tests(ieee_754).
