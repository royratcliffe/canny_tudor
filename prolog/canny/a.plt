:- begin_tests(canny_a).

:- use_module(a).

test(a_star, [true(A==[h(a, b, 1)])]) :-
    a_star([h(a, b, 1)], A, [initially(a), finally(b)]).
test(a_star, [true(A==[ h(x, d, 2),
                        h(d, e, 3),
                        h(e, y, 2)
                      ])]) :-
    a_star([ h(x, a, 1.5),
             h(a, b, 2),
             h(b, c, 3),
             h(c, y, 4),
             h(x, d, 2),
             h(d, e, 3),
             h(e, y, 2)
           ],
           A,
           [initially(x), finally(y)]).

:- end_tests(canny_a).
