:- initialization doc.

:- multifile user:file_search_path/2.

%   Add the root to the library search path. Otherwise, the
%   documentation builder reports the full path rather than the
%   library-relative path, e.g. library(SubPath). Prefer the latter.
%   Assumes that the current working directory is the *man* folder.

user:file_search_path(library, '../prolog').

%   There appears to be a problem with doc_latex/3. It back-tracks then
%   tries to load the list of input files. Work around the issue by
%   soft-cutting the redundant choice-point.

doc :-
    bagof(File, doc(File), Files),
    doc_latex(Files, 'doc.tex', [stand_alone(false), section_level(chapter)]),
    !.

doc('../README.md').
doc('../CHANGELOG.md').
doc(PL) :-
    directory_member('../prolog', PL, [file_type(prolog), recursive(true)]).
