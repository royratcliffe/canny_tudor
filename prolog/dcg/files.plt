:- begin_tests(dcg_files).
:- use_module(files).

test(directory_entry, all(Base == [endian, files])) :-
    module_property(dcg_files, file(File)),
    file_directory_name(File, Directory),
    phrase(directory_entry(Directory, _Entry), [Entry1]),
    file_name_extension(Base, pl, Entry1).

:- end_tests(dcg_files).
