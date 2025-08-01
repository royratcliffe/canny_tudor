:- begin_tests(canny_placeholders).
:- use_module(placeholders).

test(format_placeholders) :-
    format_placeholders("abc", abc, []).
test(format_placeholders) :-
    format_placeholders('abc_{id}', abc_123, [id(123)]).
test(format_placeholder, Atom-RestOptions == abc123-[id(123), rest(456)]) :-
    format_placeholders('abc{id}', Atom, [id(123), rest(456)], RestOptions).
test(format_placeholders) :-
    format_placeholders(abc, abc, [x(1)]).
test(format_placeholders) :-
    format_placeholders(abc, abc, [x(1)], [x(1)]).
test(format_placeholders, error(existence_error(matching_rule,_),context(_,_))) :-
    format_placeholders('abc{id}', _, _).

test(placeholders, Terms-Options == []-[]) :-
    phrase(placeholders(Terms, Options), ``).
test(placeholders, Terms-RestOptions =@= [a,111,b,A,c]-[name(A)]) :-
    phrase(placeholders(Terms, [id(111)|RestOptions]), `a{id}b{name}c`).
test(placeholders, Terms-Options =@= [A, B, C]-[a(A), b(B), c(C)]) :-
    phrase(placeholders(Terms, Options), `{a}{b}{c}`).
test(placeholders, Terms-Options == [ID, ID]-[id(ID)]) :-
    phrase(placeholders([], Terms, [], Options), `{id}{id}`),
    option(id(ID), Options).

:- end_tests(canny_placeholders).
