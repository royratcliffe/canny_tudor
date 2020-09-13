:- module(doc_latex, [latex_for_pack/3]).

:- ensure_loaded(library(pldoc)).

:- use_module(pldoc(doc_html)).

:- use_module(library(canny/files)).

%   There appears to be a problem with doc_latex/3. It back-tracks then
%   tries to load the list of input files. Work around the issue by
%   soft-cutting the redundant choice-point.

%   Filter   out   undocumented   Prolog   source     files.   Use   the
%   doc_file_objects/5 predicate. The Objects argument (third) should be
%   a non-empty list of objects, also  the FileOptions (fourth argument)
%   can contain file-level documentation represented   by a file/2 term.
%   Document   either   way:   documented     predicates   exist,   file
%   documentation, or both.

%!  latex_for_pack(+Spec, +OutFile, +Options) is det.

latex_for_pack(Spec, OutFile, Options) :-
    once(pack_file(Spec, PackFile)),
    file_directory_name(PackFile, Pack),
    bagof(File, latex_for_pack_(Pack, File), Files),
    doc_latex(Files, OutFile, Options),
    !.

latex_for_pack_(Pack, File) :-
    md(Spec),
    absolute_file_name(Spec, File, [extensions([md]), relative_to(Pack)]).
latex_for_pack_(Pack, File) :-
    directory_member(Pack, File0, [extensions([md]), recursive(true)]),
    absolute_file_name(File0, File),
    file_base_name(File, Name),
    file_name_extension(Base, md, Name),
    \+ md(Base).
latex_for_pack_(Pack, File) :-
    absolute_file_name(Pack/prolog, Absolute),
    directory_member(Absolute, File, [file_type(prolog), recursive(true)]),
    doc_file_objects(File, _, Objects, FileOptions, []),
    (   Objects = [_|_]
    ->  true
    ;   member(file(_, _), FileOptions)
    ).

md(readme).
md(changelog).

pack_file(Spec, File) :-
    absolute_file_name(Spec, Absolute),
    absolute_directory(Absolute, Directory),
    absolute_file_name(pack, File, [ file_type(prolog),
                                     relative_to(Directory),
                                     access(read),
                                     file_errors(fail)
                                   ]).
