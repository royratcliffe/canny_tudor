:- begin_tests(canny_arity).

:- use_module(arity).

test(arities, [fail]) :-
    arities({}, _).
test(arities, [true(A==[1])]) :-
    arities({1}, A).
test(arities, [true(A==[1, 2])]) :-
    arities({1, 2}, A).
test(arities, [true(A==[1, 2, 3])]) :-
    arities({1, 2, 3}, A).
test(arities, [true(A==[1, 2, 3, 4, 5])]) :-
    arities({1, 2, 3, 4, 5}, A).

test(arities, [fail]) :-
    arities(_, []).
test(arities, [true(A=={1})]) :-
    arities(A, [1]).
test(arities, [true(A=={1, 2})]) :-
    arities(A, [1, 2]).
test(arities, [true(A=={1, 2, 3})]) :-
    arities(A, [1, 2, 3]).

:- end_tests(canny_arity).
