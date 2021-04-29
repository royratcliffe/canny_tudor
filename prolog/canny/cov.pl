:- use_module(library(canny/pack)).
:- use_module(library(canny/cover)).
:- use_module(library(print/table)).

:- initialization(up, program).

up :-
    load_pack_modules(canny_tudor, Modules),
    print_table(
        coverage_for_modules(
            run_tests, Modules, Module, coverage{
                                            clauses:Clauses,
                                            cov:Cov,
                                            fail:Fail
                                        }), [Module, Clauses, Cov, Fail]).
