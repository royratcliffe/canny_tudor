:- begin_tests(canny_url).
:- use_module(url).

test(encoded, A == `hello`) :- url_coded(`hello`, A).
test(encoded, A == `hello%20world`) :- url_coded(`hello world`, A).
test(encoded, A == `%20`) :- url_coded(` `, A).

test(decoded, [A == `hello`]) :- url_coded(A, `hello`).
test(decoded, [A == ` `]) :- url_coded(A, `%20`).
test(decoded, fail) :- url_coded(_, `%`).
test(decoded, fail) :- url_coded(_, `%2`).

:- end_tests(canny_url).
