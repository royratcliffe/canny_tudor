:- module(with_output, [with_output_to/3, with_output_to_pl/2]).

:- use_module(library(docker/random_names)).

:- meta_predicate
    with_output_to(+, ?, 0),
    with_output_to_pl(?, 0).

%!  with_output_to(+FileType, ?Spec, :Goal) is semidet.
%
%   Runs Goal with =current_output= pointing at a file with UTF-8
%   encoding. In (+, -, :) mode, creates a randomly-generated file with
%   random new name unified at Spec.
%
%   This is an  arity-three  version   of  with_output_to/2;  same name,
%   different arity. Writes the results  of   running  Goal to some file
%   given by Spec and FileType.  Fails  if   Spec  and  FileType fail to
%   specify a writable file location.
%
%   When Spec unbound, generates a random name. Binds the name to Spec.

with_output_to(FileType, Spec, Goal) :-
    var(Spec),
    !,
    random_name_chk(Spec),
    with_output_to(FileType, Spec, Goal).
with_output_to(FileType, Spec, Goal) :-
    absolute_file_name(Spec, File, [file_type(FileType), access(write)]),
    setup_call_cleanup(
        open(File, write, Out, [encoding(utf8)]),
        with_output_to(Out, Goal),
        close(Out)).

%!  with_output_to_pl(?Spec, :Goal) is semidet.
%
%   Runs Goal with =current_output= pointing at a randomly-generated
%   Prolog source file with UTF-8 encoding. In (+, :) mode, creates a
%   Prolog file with name given by Spec.

with_output_to_pl(Spec, Goal) :-
    with_output_to(prolog, Spec, Goal).

:- multifile user:prolog_file_type/2.
:- dynamic user:prolog_file_type/2.
:- public user:prolog_file_type/2.

user:prolog_file_type(txt, text).
