:- autoload(library(aggregate)).
:- autoload(library(apply)).
:- autoload(library(lists)).
:- autoload(library(sort)).
:- autoload(library(yall)).
:- autoload(library(http/json)).
:- autoload(library(canny/pack)).
:- autoload(library(canny/cover)).
:- autoload(library(print/table)).
:- autoload(library(gh/api)).

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
    format('Fail:~t~f~40|%~n', [AvgFail]),
    (   getenv('CANNY_COV_GIST_ID', GistID)
    ->  shield_files([cov-AvgCov, fail-AvgFail], Files),
        ghapi_update_gist(GistID, json(json([files=Files])), _, [])
    ;   true
    ).

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

shield_files(Pairs, json(Files)) :-
    maplist([Label-Percent, File=json([content=Content])]>>
            (   atom_concat(Label, '.json', File),
                format(atom(Message), '~1f%', [Percent]),
                shield_color(Percent, Color),
                atom_json_term(Content, json([ schemaVersion=1,
                                               label=Label,
                                               message=Message,
                                               color=Color
                                             ]), []),
                format('raw/~s~n', [File])
            ), Pairs, Files).

shield_color(Percent, red) :- Percent < 20, !.
shield_color(Percent, orange) :- Percent < 40, !.
shield_color(Percent, yellow) :- Percent < 60, !.
shield_color(Percent, yellowgreen) :- Percent < 80, !.
shield_color(_, green).
