:- module(operating_system_file_searches, []).

:- multifile
    user:file_search_path/2.

user:file_search_path(Name, Path) :-
    current_prolog_flag(windows, true),
    name(Name),
    getenv(Name, Path),
    exists_directory(Path).

name(userprofile).
name(programfiles).
name(temp).

user:file_search_path(savedgames, userprofile('Saved Games')).
user:file_search_path(appdata, userprofile('AppData')).
user:file_search_path(applocal, appdata('Local')).
user:file_search_path(localprograms, applocal('Programs')).
