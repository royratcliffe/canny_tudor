% Prolog test code for Docker operations in the "Canny" project. This
% file provides predicates to interact with Docker. It includes tests to
% ensure functionality works as expected. The tests only run if the
% Docker system is available. In other words, run these tests only if
% Docker is installed and running on the test system. The API
% system_ping is used to check Docker's availability.
:- begin_tests(canny_docker, [condition(docker(system_ping, 'OK', []))]).
:- use_module(docker).
:- use_module(library(http/json)).

test(container_list) :-
    docker(container_list, JSON, []),
    % Check if the JSON response is a valid JSON term. An empty list is a valid
    % response since `is_json_term([])` succeeds.
    is_json_term(JSON).
test(container_list) :-
    docker(container_list, Dicts, [json_object(dict)]),
    maplist(is_dict, Dicts).

:- end_tests(canny_docker).
