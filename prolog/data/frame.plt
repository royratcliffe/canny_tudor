:- begin_tests(data_frame).

:- use_module(frame).

test(columns_to_rows, [true(A==[[a-1, b-3], [a-2, b-4]])]) :-
    columns_to_rows([a=[1, 2], b=[3, 4]], A).
test(columns_to_rows, [throws(error(instantiation_error, _))]) :-
    columns_to_rows(_, [[a-1, b-3], [a-2, b-4]]).

:- end_tests(data_frame).
