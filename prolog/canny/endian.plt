:- begin_tests(canny_endian).

:- use_module(endian).

test(byte, [fail]) :-
    byte(_, [256], []).
test(byte, [true(A==255)]) :-
    byte(A, [255], []).

test(big, [true(A-B==256-[0])]) :-
    phrase(big_endian(16, A), [1, 0, 0], B).
test(big, [true(B-C=@=[1, 1|A]-A)]) :-
    phrase(big_endian(16, 257), B, C).
test(big, [true(B==[1, 1])]) :-
    phrase(big_endian(16, 257), B).

test(big, [true(A==[0, 1, 0, 0])]) :-
    phrase(big_endian(32, 1<<16), A, []).
test(big, [true(A==[0, 0, 255, 255])]) :-
    phrase(big_endian(32, 1<<16-1), A, []).

test(big, [fail]) :-
    phrase(big_endian(32, 1<<32), _, []).

test(little_endian, [true(A==0x01020304)]) :-
    phrase(little_endian(32, A), [4, 3, 2, 1]).
test(little_endian, [true(B==[4, 3, 2, 1])]) :-
    phrase(little_endian(32, A), [4, 3, 2, 1]),
    phrase(little_endian(32, A), B).

:- end_tests(canny_endian).
