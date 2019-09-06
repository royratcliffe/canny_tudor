:- begin_tests(docker_random_names).

:- use_module(random_names).

:- public test/2.

test(random_name, [fail]) :-
    random_name(x).
test(random_name, []) :-
    random_name(admiring_albattani).

:- end_tests(docker_random_names).
