:- module(canny_files, [absolute_directory/2]).

%!  absolute_directory(+Absolute, -Directory) is nondet.
%
%   Finds the directories of Absolute by   walking  up the absolute path
%   until it reaches the root. Operates on paths only; it does not check
%   that Absolute actually exists. Absolute can   be a directory or file
%   path.
%
%   Fails if Absolute  is  not  an   absolute  file  name,  according to
%   is_absolute_file_name/2. Works correctly for Unix and Windows paths.
%   However, it finally unifies with the drive letter under Windows, and
%   the root directory (/) on Unix.
%
%   @arg Absolute specifies an absolute path   name.  On Windows it must
%   typically include a driver letter, else not absolute in the complete
%   sense  under  Microsoft  Windows  since  its  file  system  supports
%   multiple root directories on different mounted drives.

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
