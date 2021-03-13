:- begin_tests(canny_pop).

:- use_module(pop).

test(pop_lsbs, [true(A==[0])]) :- pop_lsbs(1, A).
test(pop_lsbs, [true(A==[0, 2])]) :- pop_lsbs(5, A).
test(pop_lsbs, [true(A==[1, 2])]) :- pop_lsbs(6, A).
test(pop_lsbs, [true(A==[0, 1, 2, 3])]) :- pop_lsbs(15, A).
test(pop_lsbs, [true(A==[0, 1, 3])]) :- pop_lsbs(15-4, A).
test(pop_lsbs, [true(A==[2, 5, 6])]) :- pop_lsbs(100, A).
test(pop_lsbs, [throws(error(domain_error(not_less_than_one, -1), _))]) :-
    pop_lsbs(-1, _).

:- end_tests(canny_pop).
