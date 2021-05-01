:- use_module(library(canny/pack)).
:- use_module(library(canny/cover)).
:- use_module(library(print/table)).

:- initialization(cov).

cov :-
    module_coverages(ModuleCoverages),
    print_module_coverages(ModuleCoverages),
    aggregate_all(
        all(sum(Clauses), sum(Cov), sum(Fail), count),
        member(_-coverage{
                     clauses:Clauses,
                     cov:Cov,
                     fail:Fail
                 }, ModuleCoverages),
        all(AllClauses, AllCov, AllFail, AllModule)),
    AvgCov is AllCov / AllModule,
    AvgFail is AllFail / AllModule,
    format('Modules:~t~d~40|~n', [AllModule]),
    format('Clauses:~t~d~40|~n', [AllClauses]),
    format('Cov:~t~f~40|%~n', [AvgCov]),
    format('Fail:~t~f~40|%~n', [AvgFail]).

module_coverages(ModuleCoverages) :-
    load_pack_modules(canny_tudor, Modules),
    findall(
        Module-Coverage,
        coverage_for_modules(run_tests, Modules, Module, Coverage),
        ModuleCoverages).

print_module_coverages(ModuleCoverages) :-
    predsort(compare_cov_fail, ModuleCoverages, SortedModuleCoverages),
    print_table(
        member(
            Module-coverage{
                       clauses:Clauses,
                       cov:Cov,
                       fail:Fail
                   }, SortedModuleCoverages), [Module, Clauses, Cov, Fail]).

compare_cov_fail(Order, _-Coverage1, _-Coverage2) :-
    compare(Order0, Coverage1.cov, Coverage2.cov),
    compare_fail(Order, Order0, Coverage1.fail, Coverage2.fail),
    !.
compare_cov_fail(>, _, _).

compare_fail(<, <, _, _) :- !.
compare_fail(<, =, Fail1, Fail2) :- compare(>, Fail1, Fail2), !.
compare_fail(>, _, _, _).
