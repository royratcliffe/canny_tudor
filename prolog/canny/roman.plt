:- begin_tests(roman).
:- use_module(roman).

test(roman_0, [true(A==``)]) :- roman_number(A, 0).

test(roman_5, [true(A==`V`)]) :- roman_number(A, 5).

test(roman_9999, [true(A==`MMMMMMMMMCMXCIX`)]) :- roman_number(A, 9999).

test(error, [throws(error(domain_error(clpfd_expression, 5.1), _))]) :-
    roman_number(_, 5.1).

:- end_tests(roman).
