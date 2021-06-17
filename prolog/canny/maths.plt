:- begin_tests(canny_maths).

:- use_module(maths).

test(remainder) :- frem(5.1, 3, Z), epsilon_equal(2, Z, -0.9).
test(remainder) :- frem(-5.1, 3, Z), epsilon_equal(2, Z, 0.9).
test(remainder) :- frem(5.1, -3, Z), epsilon_equal(2, Z, -0.9).
test(remainder) :- frem(-5.1, -3, Z), epsilon_equal(2, Z, 0.9).

test(fmod) :- fmod(5.1, 3, Z), epsilon_equal(2, Z, 2.1).
test(fmod) :- fmod(-5.1, 3, Z), epsilon_equal(2, Z, -2.1).
test(fmod) :- fmod(5.1, -3, Z), epsilon_equal(2, Z, 2.1).
test(fmod) :- fmod(-5.1, -3, Z), epsilon_equal(2, Z, -2.1).

frld(A, B) :-
    frexp(A, Y, Exp),
    ldexp(Y, B, Exp).

frld(A) :- frld(A, A).

test(frexp_ldexp) :- frld(1.1).
test(frexp_ldexp) :- frld(123.123).

test(frexp, [true(A-B==C)]) :-
    frexp(1.0Inf, A, B),
    (   current_os(win64)
    ->  C = 1.5NaN- -1
    ;   C = 1.0Inf-0
    ).

:- end_tests(canny_maths).
