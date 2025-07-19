:- begin_tests(canny_placeholders).
:- use_module(placeholders).

test(format_placeholders) :-
    format_placeholders("abc", abc, []).
test(format_placeholders) :-
    format_placeholders('abc_{id}', abc_123, [id(123)]).
test(format_placeholders, fail) :-
    format_placeholders(abc, _, [x(1)]).

test(format_placeholders, Terms-Options == []-[]) :-
    phrase(format_placeholders(Terms, Options), ``).
test(format_placeholders, Terms-RestOptions =@= [a,111,b,A,c]-[name(A)]) :-
    phrase(format_placeholders(Terms, [id(111)|RestOptions]), `a{id}b{name}c`).

:- end_tests(canny_placeholders).
