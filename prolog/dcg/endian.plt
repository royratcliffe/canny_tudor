:- begin_tests(dcg_endian).
:- use_module(endian).

test(endian, [true(v(A, B, C, D)==v(16, [C, D], C, D))]) :-
    dcg_endian:endianness(A, B, [C, D], []).

test(big_endian, [true(v(A, B) == v(16, 16'ff00))]) :-
    phrase(dcg_endian:big_endian(A, B), [255, 0]).

test(big_endian_, [true(v(A, B, C)==v(0, 100, 0))]) :-
    dcg_endian:big_endian_([A, B], 100, C).

test(little_endian, [true(v(A, B) == v(16, 16'ff))]) :-
    phrase(dcg_endian:little_endian(A, B), [255, 0]).

test(little_endian_, [true(v(A, B, C)==v(100, 0, 0))]) :-
    dcg_endian:little_endian_([A, B], 100, C).

:- end_tests(dcg_endian).
