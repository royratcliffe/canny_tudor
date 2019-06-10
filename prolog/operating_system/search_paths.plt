:- begin_tests(operating_system_search_paths).

:- use_module(search_paths).

test(search_path) :-
    search_path('PATH', Path),
    is_of_type(list, Path).

:- end_tests(operating_system_search_paths).
