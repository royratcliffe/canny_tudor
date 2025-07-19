:- begin_tests(canny_docker, [condition(docker(system_ping, 'OK', []))]).
:- use_module(docker).
:- use_module(library(http/json)).

test(container_list) :-
    docker(container_list, JSON, []),
    is_json_term(JSON).

:- end_tests(canny_docker).
