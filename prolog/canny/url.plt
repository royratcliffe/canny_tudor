:- begin_tests(canny_url).
:- use_module(url).

test(encoded, A == `hello`) :- url_coded(`hello`, A).
test(encoded, A == `hello%20world`) :- url_coded(`hello world`, A).
test(encoded, A == `%20`) :- url_coded(` `, A).
test(encoded, A == `%ff`) :- url_coded(`\xff`, A).

test(decoded, [A == `hello`]) :- url_coded(A, `hello`).
test(decoded, [A == ` `]) :- url_coded(A, `%20`).
test(decoded, fail) :- url_coded(_, `%`).
test(decoded, fail) :- url_coded(_, `%2`).
test(encoded, A == `\xff`) :- url_coded(A, `%FF`).
test(encoded, A == `\xff`) :- url_coded(A, `%ff`).

test(coded, Encoded == `Hello_World%21`) :- url_coded(`Hello_World!`, Encoded).
test(coded, Decoded == `Hello_World!`) :- url_coded(Decoded, `Hello_World%21`).

:- end_tests(canny_url).
