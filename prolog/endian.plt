:- begin_tests(endian).

:- use_module(endian).

test(byte, [fail]) :-
    byte(_, [256], []).
test(byte, [true(A==255)]) :-
    byte(A, [255], []).

test(big, [true(A-B==256-[0])]) :-
    phrase(big(16, A), [1, 0, 0], B).
test(big, [true(B-C=@=[1, 1|A]-A)]) :-
    phrase(big(16, 257), B, C).
test(big, [true(B==[1, 1])]) :-
    phrase(big(16, 257), B).

test(big, [true(A==[0, 1, 0, 0])]) :-
    big(32, 1<<16, A, []).
test(big, [true(A==[0, 0, 255, 255])]) :-
    big(32, 1<<16-1, A, []).

test(big, [fail]) :-
    big(32, 1<<32, _, []).

:- end_tests(endian).
