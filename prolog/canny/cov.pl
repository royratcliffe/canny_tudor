:- use_module(library(canny/pack)).
:- use_module(library(canny/cover)).
:- use_module(library(print/table)).

:- initialization(cov).

cov :-
    covs(Covs),
    print_table(
        member(
            Module-coverage{
                       clauses:Clauses,
                       cov:Cov,
                       fail:Fail
                   }, Covs), [Module, Clauses, Cov, Fail]),
    aggregate_all(
        v(sum(Clauses), sum(Cov), sum(Fail), count),
        member(_-coverage{clauses:Clauses, cov:Cov, fail:Fail}, Covs),
        v(AllClauses, AllCov, AllFail, AllModule)),
    AvgCov is AllCov / AllModule,
    AvgFail is AllFail / AllModule,
    format('Modules:~t~d~40|~n', [AllModule]),
    format('Clauses:~t~d~40|~n', [AllClauses]),
    format('Cov:~t~f~40|%~n', [AvgCov]),
    format('Fail:~t~f~40|%~n', [AvgFail]).

covs(Covs) :-
    load_pack_modules(canny_tudor, Modules),
    findall(
        Module-Coverage,
        coverage_for_modules(run_tests, Modules, Module, Coverage),
        Covs).
