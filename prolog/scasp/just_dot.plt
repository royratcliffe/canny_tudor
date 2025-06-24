:- begin_tests(scasp_just_dot).
:- use_module(just_dot).

:- multifile user:file_search_path/2.

user:file_search_path(scasp_just_dot_test, Path) :-
    context_module(M),
    source_file_property(SourceFile, module(M)),
    directory_file_path(Directory, _, SourceFile),
    absolute_file_name(test, Path, [relative_to(Directory)]).

test_file(File, Abs) :-
    absolute_file_name(scasp_just_dot_test(File), Abs).

test(citizen, JsonCodes == DotCodes) :-
    test_file('citizen.json', AbsJson),
    test_file('citizen.dot', AbsDot),
    with_output_to(codes(JsonCodes),
                   scasp_just_dot_print(current_output, AbsJson, [])),
    format('~s', [JsonCodes]),
    read_file_to_codes(AbsDot, DotCodes, []).

:- end_tests(scasp_just_dot).
