:- begin_tests(swi_codes).

:- use_module(codes).

test(split_lines, [true(A==[[]])]) :-
    split_lines([0'\n], A).
test(split_lines, [true(A==[0'\n])]) :-
    split_lines(A, [[]]).
test(split_lines, [true(A==[0'\n, 0'\n])]) :-
    split_lines(A, [[], []]).
test(split_lines, [true(A-B=="hello"-"world")]) :-
    string_codes("hello\nworld", Codes),
    split_lines(Codes, [Line1, Line2]),
    string_codes(A, Line1),
    string_codes(B, Line2).
test(split_lines, [true(A=="hello\nworld\n")]) :-
    string_codes("hello", Line1),
    string_codes("world", Line2),
    split_lines(Codes, [Line1, Line2]),
    string_codes(A, Codes).

:- end_tests(swi_codes).
