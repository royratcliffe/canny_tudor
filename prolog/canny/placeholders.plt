:- begin_tests(canny_placeholders).
:- use_module(placeholders).

test(format_path, Terms-RestOptions =@= [a,111,b,A,c]-[name(A)]) :-
    phrase(canny_placeholders:format_path(Terms, [id(111)|RestOptions]), `a{id}b{name}c`).

:- end_tests(canny_placeholders).
