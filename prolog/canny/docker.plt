% Prolog test code for Docker operations in Canny project. This file provides
% predicates to interact with Docker. It includes tests to ensure functionality
% works as expected. The tests only run if the Docker system is available. In
% other words, run these tests only if Docker is installed and running on the
% test system. The API system_ping is used to check Docker's availability.
:- begin_tests(canny_docker, [condition(docker(system_ping, 'OK', []))]).
:- use_module(docker).
:- use_module(library(http/json)).

test(container_list) :-
    docker(container_list, JSON, []),
    is_json_term(JSON).

:- end_tests(canny_docker).
