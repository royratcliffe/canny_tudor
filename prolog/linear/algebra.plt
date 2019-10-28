:- begin_tests(linear_algebra).

:- use_module(algebra).

:- public test/2.

test(vector_heading, [true(A==[0.7071067811865476, 0.7071067811865475])]) :-
    vector_heading(A, 45*pi/180).

:- end_tests(linear_algebra).
