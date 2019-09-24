:- module(random_temporary, [random_temporary_module/1]).

%!  random_temporary_module(-M:atom) is nondet.
%
%   Finds a module that does not exist.   Makes it exist. The new module
%   has a module class of   temporary. Operates non-deterministically by
%   continuously generating a newly unique   temporary  module. Surround
%   with once/1 when generating just a single module.
%
%   Utilises the uuid/1 predicate which  never fails; the implementation
%   relies on that prerequisite. Nor  does uuid/1 automatically generate
%   a  randomly  _unique_  identifier.  The  implementation  repeats  on
%   failure to find a  module  that  does   not  already  exist.  If the
%   generation of a new unique module   name always fails, the predicate
%   will  continue  an  infinite  failure-driven    loop  running  until
%   interrupted within the calling thread.
%
%   The predicate allows for concurrency by operating a mutex across the
%   clauses testing for an existing module   and  its creation. Succeeds
%   only for mode (-).

random_temporary_module(M) :-
    var(M),
    repeat,
    uuid(M),
    with_mutex(random_temporary_module, unique_module(M)).

unique_module(M) :-
    \+ current_module(M),
    set_module(M:class(temporary)).
