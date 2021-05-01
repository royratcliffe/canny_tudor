:- module(canny_pack,
          [ load_pack_modules/2,
            load_prolog_source/2
          ]).
:- autoload(library(filesex), [directory_member/3]).
:- autoload(library(prolog_pack), [pack_property/2]).
:- use_module(library(plunit), [load_test_files/1]).

load_pack_modules(Pack, Modules) :-
    pack_property(Pack, directory(Directory)),
    findall(Module, load_prolog_source(Directory, Module), Modules),
    load_test_files([]).

load_prolog_source(Directory, Module) :-
    directory_member(Directory, File, [file_type(prolog), recursive(true)]),
    catch(load_files(File, [must_be_module(true)]), _, fail),
    module_property(Module, file(File)).
