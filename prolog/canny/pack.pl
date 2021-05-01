:- module(canny_pack,
          [ load_pack_modules/2,
            load_prolog_source/2
          ]).
:- autoload(library(filesex), [directory_member/3]).
:- autoload(library(prolog_pack), [pack_property/2]).
:- use_module(library(plunit), [load_test_files/1]).

%!  load_pack_modules(+Pack, -Modules) is semidet.
%
%   Finds and loads all Prolog module sources  for Pack. Also loads test
%   files having once  loaded  the  pack.   Modules  becomes  a  list of
%   successfully-loaded pack modules.

load_pack_modules(Pack, Modules) :-
    pack_property(Pack, directory(Directory)),
    findall(Module, load_prolog_source(Directory, Module), Modules),
    load_test_files([]).

%!  load_prolog_source(+Directory, -Module) is nondet.
%
%   Loads Prolog source recursively at Directory  for Module. Does *not*
%   load non-module sources, e.g.  scripts   without  a module. Operates
%   non-deterministically for Module. Finds and   loads  all the modules
%   within  a  given  directory;  typically  amounts   to  a  pack  root
%   directory. You can find the File from  which the module loaded using
%   module properties, i.e. `module_property(Module, file(File))`.

load_prolog_source(Directory, Module) :-
    directory_member(Directory, File, [file_type(prolog), recursive(true)]),
    catch(load_files(File, [must_be_module(true)]), _, fail),
    module_property(Module, file(File)).
