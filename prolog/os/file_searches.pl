:- module(os_file_searches, []).

/** <module> File searches
 *
 * By design, the following extensions for Windows avoid underscores
 * in order not to clash with existing standard paths, e.g. =app_path=
 * which Prolog defines by default.
 *
 */

:- multifile
    user:file_search_path/2.

user:file_search_path(Name, Path) :-
    current_prolog_flag(windows, true),
    name(Name),
    getenv(Name, Path),
    exists_directory(Path).

name(userprofile).
name(onedrive).
name(onedrivecommercial).
name(onedrivepersonal).
name(programfiles).
name(temp).

user:file_search_path(documents, userprofile('Documents')).
user:file_search_path(savedgames, userprofile('Saved Games')).
user:file_search_path(appdata, userprofile('AppData')).
user:file_search_path(applocal, appdata('Local')).
user:file_search_path(localprograms, applocal('Programs')).
