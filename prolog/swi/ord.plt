:- begin_tests(ord).

:- use_module(ord).

test(iud, [true(v(A, B, C)==v([], [], [a, b, c]))]) :-
    iud([a, b, c], [], A, B, C).
test(iud, [true(v(A, B, C)==v([c], [], [a, b]))]) :-
    iud([a, b], [c], A, B, C).
test(iud, [true(v(A, B, C)==v([b, c], [], [a]))]) :-
    iud([a], [b, c], A, B, C).
test(iud, [true(v(A, B, C)==v([c], [b], [a]))]) :-
    iud([a, b], [b, c], A, B, C).
test(iud, [true(v(A, B, C)==v([a, b, c], [], []))]) :-
    iud([], [a, b, c], A, B, C).

:- end_tests(ord).
