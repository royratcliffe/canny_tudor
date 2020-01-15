:- begin_tests(canny_maths).

:- use_module(maths).

:- public test/1, test/2.

test(remainder) :- remainder(5.1, 3, Z), epsilon_equal(2, Z, -0.9).
test(remainder) :- remainder(-5.1, 3, Z), epsilon_equal(2, Z, 0.9).
test(remainder) :- remainder(5.1, -3, Z), epsilon_equal(2, Z, -0.9).
test(remainder) :- remainder(-5.1, -3, Z), epsilon_equal(2, Z, 0.9).

test(fmod) :- fmod(5.1, 3, Z), epsilon_equal(2, Z, 2.1).
test(fmod) :- fmod(-5.1, 3, Z), epsilon_equal(2, Z, -2.1).
test(fmod) :- fmod(5.1, -3, Z), epsilon_equal(2, Z, 2.1).
test(fmod) :- fmod(-5.1, -3, Z), epsilon_equal(2, Z, -2.1).

test(permute_sum, [all(A==[[1, 1, 1], [1, 2], [2, 1], [3]])]) :-
    permute_sum(3, A).

:- end_tests(canny_maths).
