:- module(power_shell_get_content, []).

:- use_module(library(os/apps), []).

:- multifile
    os:app_property/2.

os:app_property(
       power_shell:get_content(Spec, _),
       path(path(powershell))) :-
    absolute_file_name(Spec, _, [access(read)]).
os:app_property(
       power_shell:get_content(_, _),
       argument('Get-Content')).
os:app_property(
       power_shell:get_content(Spec, _),
       argument(Argument)) :-
    absolute_file_name(Spec, Path0, [access(read)]),
    prolog_to_os_filename(Path0, Path_),
    format(atom(Path), '"~s"', [Path_]),
    member(Argument, ['-Path', Path]).
os:app_property(
       power_shell:get_content(_, Options),
       argument(Argument)) :-
    member(Option, Options),
    option_argument(Option, Argument).
os:app_property(
       power_shell:get_content(_, Options),
       option(encoding(Encoding0))) :-
    member(encoding(Encoding0), Options),
    encoding_argument(Encoding0, _).

option_argument(tail(_), '-Tail').
option_argument(tail(Lines), Lines).
option_argument(encoding(_), '-Encoding').
option_argument(encoding(Encoding0), Encoding) :-
    encoding_argument(Encoding0, Encoding).
option_argument(wait, '-Wait').

encoding_argument(utf8, 'UTF8').
