:- begin_tests(linear_algebra).

:- use_module(algebra).

:- public test/2.

test(matrix_identity, [nondet]) :-
    matrix_identity(0, []).
test(matrix_identity, [true(A==[[1]]), nondet]) :-
    matrix_identity(1, A).
test(matrix_identity, [true(A==[[1, 0], [0, 1]]), nondet]) :-
    matrix_identity(2, A).
test(matrix_identity, [true(A==[[1, 0, 0], [0, 1, 0], [0, 0, 1]]), nondet]) :-
    matrix_identity(3, A).

test(vector_heading, [true(A==[0.7071067811865476, 0.7071067811865475])]) :-
    vector_heading(A, 45*pi/180).

:- end_tests(linear_algebra).
