:- module(canny_files, []).

absolute_directory(Absolute, Directory) :-
    is_absolute_file_name_or_root(Absolute),
    absolute_directory_(Absolute, Directory).

absolute_directory_(Absolute, Absolute).
absolute_directory_(Absolute, Directory) :-
    file_directory_name_unless_root(Absolute, Directory_),
    absolute_directory(Directory_, Directory).

is_absolute_file_name_or_root(/) :- !.
is_absolute_file_name_or_root(File) :- is_absolute_file_name(File).

file_directory_name_unless_root(/, _Directory) :- !, fail.
file_directory_name_unless_root(File, Directory) :-
    file_directory_name(File, Directory).
