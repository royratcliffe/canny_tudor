:- module(power_shell_get_content, []).

:- use_module(library(os/apps), []).

:- multifile os:property_for_app/2.

os:property_for_app(path(path(powershell)), App) :-
    path_options(App, _, _).

path_options(power_shell:get_content(Spec, Options), Path, Options) :-
    ground(Spec),
    absolute_file_name(Spec, Path, [access(read)]).

os:property_for_app(argument('Get-Content'), App) :-
    path_options(App, _, _).

%   Important to wrap the Path argument in double quotes for PowerShell,
%   even though passed through process_create/3 as a distinct argument.
%   Fails without double quotes if the path includes spaces.

os:property_for_app(argument(Argument), App) :-
    path_options(App, Path0, _),
    prolog_to_os_filename(Path0, Path_),
    format(atom(Path), '"~s"', [Path_]),
    member(Argument, ['-Path', Path]).

os:property_for_app(argument(Argument), App) :-
    path_options(App, _, Options),
    member(Option, Options),
    option_argument(Option, Argument).

option_argument(tail(_), '-Tail').
option_argument(tail(Lines), Lines).
option_argument(encoding(_), '-Encoding').
option_argument(encoding(Encoding0), Encoding) :-
    encoding_argument(Encoding0, Encoding).
option_argument(wait, '-Wait').

os:property_for_app(option(encoding(Encoding0)), App) :-
    path_options(App, _, Options),
    member(encoding(Encoding0), Options),
    encoding_argument(Encoding0, _).

encoding_argument(utf8, 'UTF8').
