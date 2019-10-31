:- begin_tests(linear_algebra).

:- use_module(algebra).

:- public test/2.

test(matrix_dimensions, []) :-
    matrix_dimensions(_, 0, 0).

test(matrix_dimensions, [true(A-B=@=0-_), nondet]) :-
    matrix_dimensions([], A, B).
test(matrix_dimensions, [true(A-B==1-0), nondet]) :-
    matrix_dimensions([[]], A, B).
test(matrix_dimensions, [true(A-B=@=[]-_), nondet]) :-
    matrix_dimensions(A, 0, B).

test(matrix_dimensions, [true(A=@=[[_, _, _, _],
                                   [_, _, _, _],
                                   [_, _, _, _],
                                   [_, _, _, _]]), nondet]) :-
    matrix_dimensions(A, 4, 4).

test(matrix_identity, []) :-
    matrix_identity(0, []).
test(matrix_identity, [true(A==[[1]])]) :-
    matrix_identity(1, A).
test(matrix_identity, [true(A==[[1, 0], [0, 1]])]) :-
    matrix_identity(2, A).
test(matrix_identity, [true(A==[[1, 0, 0], [0, 1, 0], [0, 0, 1]])]) :-
    matrix_identity(3, A).

test(matrix_transpose, [fail]) :-
    matrix_transpose([], _).
test(matrix_transpose, [true(A==[[1, 3], [2, 4]])]) :-
    matrix_transpose([[1, 2], [3, 4]], A).
test(matrix_transpose, [true(A==[[1, 4, 7], [2, 5, 8], [3, 6, 9]])]) :-
    matrix_transpose(A, [[1, 2, 3], [4, 5, 6], [7, 8, 9]]).

test(matrix_transpose, [true(A==[[1], [2], [3]])]) :-
    matrix_transpose([[1, 2, 3]], A).
test(matrix_transpose, [true(A==[[1, 2, 3]])]) :-
    matrix_transpose(A, [[1], [2], [3]]).

test(matrix_multiply, [true(A==[[58.0, 64.0], [139.0, 154.0]]), nondet]) :-
    matrix_multiply([[1, 2, 3], [4, 5, 6]], [[7, 8], [9, 10], [11, 12]], A).

test(vector_heading, [true(A==[0.7071067811865476, 0.7071067811865475])]) :-
    vector_heading(A, 45*pi/180).

:- end_tests(linear_algebra).
