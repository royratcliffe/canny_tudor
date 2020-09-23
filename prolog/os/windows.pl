:- module(os_windows, []).

/** <module> Microsoft Windows Operating System
 *
 * By design, the following extensions for Windows avoid underscores
 * in order not to clash with existing standard paths, e.g. =app_path=
 * which Prolog defines by default.
 *
 *     userprofile
 *     onedrive
 *     onedrivecommercial
 *     onedrivepersonal
 *     programfiles
 *     temp
 *     documents
 *     savedgames
 *     appdata
 *     applocal
 *     localprograms
 *
 */

:- multifile user:file_search_path/2.

name_path(Name, Path) :-
    current_prolog_flag(windows, true),
    getenv(Name, Path),
    exists_directory(Path).

user:file_search_path(userprofile, Path) :- name_path(userprofile, Path).
user:file_search_path(onedrive, Path) :- name_path(onedrive, Path).
user:file_search_path(onedrivecommercial, Path) :-
    name_path(onedrivecommercial, Path).
user:file_search_path(onedrivepersonal, Path) :-
    name_path(onedrivepersonal, Path).
user:file_search_path(programfiles, Path) :- name_path(programfiles, Path).
user:file_search_path(temp, Path) :- name_path(temp, Path).

user:file_search_path(documents, userprofile('Documents')).
user:file_search_path(savedgames, userprofile('Saved Games')).
user:file_search_path(appdata, userprofile('AppData')).
user:file_search_path(applocal, appdata('Local')).
user:file_search_path(localprograms, applocal('Programs')).
