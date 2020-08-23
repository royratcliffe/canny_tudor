:- begin_tests(ieee_754).

:- use_module('754').

:- use_module(library(canny/maths)).

pack_unpack(Width, Float0, Float) :-
    ieee_754_float(Width, Word, Float0),
    ieee_754_float(Width, Word, Float).

test(pack_unpack) :- pack_unpack(32, 3.0, A), epsilon_equal(3.0, A).
test(pack_unpack) :- pack_unpack(32, 0.0, A), epsilon_equal(0.0, A).
test(pack_unpack) :- pack_unpack(32, -3.0, A), epsilon_equal(-3.0, A).

:- end_tests(ieee_754).
