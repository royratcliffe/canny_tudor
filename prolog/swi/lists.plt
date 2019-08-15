:- begin_tests(swi_lists).

:- use_module(lists).

test(zip) :-
    zip([], [], []),
    zip([1], [2], [[1, 2]]),
    zip([1, a], [2, b], [[1, 2], [a, b]]).

test(zip, [true(A-B==[1, 3]-[2, 4])]) :-
    zip(A, B, [[1, 2], [3, 4]]).

:- end_tests(swi_lists).
