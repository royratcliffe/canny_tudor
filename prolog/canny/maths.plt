:- begin_tests(canny_maths).

:- use_module(maths).

test(remainder) :- remainder(5.1, 3, Z), epsilon_equal(2, Z, -0.9).
test(remainder) :- remainder(-5.1, 3, Z), epsilon_equal(2, Z, 0.9).
test(remainder) :- remainder(5.1, -3, Z), epsilon_equal(2, Z, -0.9).
test(remainder) :- remainder(-5.1, -3, Z), epsilon_equal(2, Z, 0.9).

test(fmod) :- fmod(5.1, 3, Z), epsilon_equal(2, Z, 2.1).
test(fmod) :- fmod(-5.1, 3, Z), epsilon_equal(2, Z, -2.1).
test(fmod) :- fmod(5.1, -3, Z), epsilon_equal(2, Z, 2.1).
test(fmod) :- fmod(-5.1, -3, Z), epsilon_equal(2, Z, -2.1).

:- end_tests(canny_maths).
