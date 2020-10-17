:- use_module(library(doc/latex)).

:- initialization doc.

doc :- latex_for_pack(., 'doc.tex', [ stand_alone(false),
                                      section_level(chapter)
                                    ]).

:- multifile user:file_search_path/2.

%   Add the root to the library search path. Otherwise, the
%   documentation builder reports the full path rather than the
%   library-relative path, e.g. library(SubPath). Prefer the latter.
%   Assumes that the current working directory is the *man* folder.

user:file_search_path(library, '../prolog').
