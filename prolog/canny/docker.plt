:- begin_tests(canny_docker).
:- use_module(docker).

test(format_path, Terms-RestOptions =@= [a,111,b,A,c]-[name(A)]) :-
    phrase(canny_docker:format_path(Terms, [id(111)|RestOptions]), `a{id}b{name}c`).

:- end_tests(canny_docker).
