:- begin_tests(canny_pop).

:- use_module(pop).

test(poplsbs, [true(A==[0])]) :- poplsbs(1, A).
test(poplsbs, [true(A==[0, 2])]) :- poplsbs(5, A).
test(poplsbs, [true(A==[1, 2])]) :- poplsbs(6, A).
test(poplsbs, [true(A==[0, 1, 2, 3])]) :- poplsbs(15, A).
test(poplsbs, [true(A==[0, 1, 3])]) :- poplsbs(15-4, A).
test(poplsbs, [true(A==[2, 5, 6])]) :- poplsbs(100, A).
test(poplsbs, [throws(error(domain_error(not_less_than_one, -1), _))]) :-
    poplsbs(-1, _).

:- end_tests(canny_pop).
