:- module(canny_situations, [for_situation/2, situation_property/2]).

:- meta_predicate
    for_situation(?, :),                % ?For, ?Situation
    situation_property(:, ?).           % ?Situation, ?Property

:- multifile
    canny:for_situation/2,
    canny:property_of_situation/2.

:- use_module(library(random/temporary)).

:- dynamic situation_module/2.

%!  for_situation(?For, ?Situation:any) is nondet.
%
%   Apply term For to Situation, where For is one of the following. Note
%   that the For term may be nonground.  It can contain variables if the
%   situation mutation generates new information.
%
%       * module(?Module)
%
%       Sets  up  Situation  using  Module.    Establishes  the  dynamic
%       predicate options for the temporary   situation  module used for
%       persisting situation Now-At and Was-When tuples.
%
%       An important side effect occurs for  ground Situation terms. The
%       implementation creates the  situation's   temporary  module  and
%       applies default options  to  its   new  dynamic  predicates. The
%       module(Module) term unifies with the   newly-created or existing
%       situation module.
%
%       The predicate's determinism collapses   to  semi-determinism for
%       ground situations. Otherwise with variable Situation components,
%       the predicate unifies with  all   matching  situations, unifying
%       with module(Module) non-deterministically.
%
%       * now(+Now:any, +At:number)
%
%       Makes some Situation become Now for time   index At, at the next
%       fixation. Effectively schedules a  pending   update  one or more
%       times; the next situation =fix/0=   fixes  the pending situation
%       changes at some future point.
%
%       Uses canny:for_situation/2 when Situation is   ground,  but uses
%       canny:property_of_situation/2 otherwise. Asserts   therefore for
%       multiple situations if Situation comprises variables. You cannot
%       therefore have non-ground situations.
%
%       * fix
%
%       Fixating situations does three important  things. First, it adds
%       new Previous-When pairs to the   situation  history. They become
%       =was/2= dynamic facts (clauses without  rules). Second, it adds,
%       replaces or removes the  most   current  Current-When pair. This
%       allows detection of non-events, e.g.  when something disappears.
%       Some types of situation might require such event edges. Finally,
%       fixating broadcasts situation-change messages.
%
%       The rule for fixing the  Current-When   pair  goes like this: Is
%       there a new =now/2=, at least one?   The  latest becomes the new
%       current.  Any  others  become  Previous-When.  If  there  is  no
%       =now/2=,  then  the  current    disappears.  Messages  broadcast
%       accordingly. If there is more than  one =now/2=, only the latest
%       becomes current. Hence  currently-previously   only  transitions
%       once in-between fixations.
%
%       * fix(+Now:any)
%
%       Shortcut for now(Now, At) and fix where =At= becomes the current
%       Unix epoch time. Fixes but does not retract history terms.
%
%       * retract(+When:number)
%       * retract(?When:number, +Delay:number)
%
%       Retracts all =was/2= clauses for   all matching Situation terms.
%       Term retract(_, Delay) retracts all  =was/2= history terms using
%       the last term's latest time stamp. In  this way, you can retract
%       situations without knowing their absolute time. For example, you
%       can retract everything older than 60 seconds from the last known
%       history term when you retract(_, 60).

for_situation(For, Situation) :-
    canny:for_situation(For, Situation).

canny:for_situation(module(Module), Situation) :-
    with_mutex(canny_situations, temporary_module(Situation, Module)).

temporary_module(Situation, Module) :-
    situation_module(Situation, Module),
    !.
temporary_module(Situation, Module) :-
    ground(Situation),
    once(random_temporary_module(Module)),
    asserta(situation_module(Situation, Module)),
    dynamic([   Module:now/2,
                Module:was/2,
                Module:currently/2,
                Module:previously/2
            ], []).

canny:for_situation(now(Now, At), Situation) :-
    ground(Now),
    number(At),
    now(Situation, Now, At).
canny:for_situation(now(Now), Situation) :-
    get_time(At),
    canny:for_situation(now(Now, At), Situation).

now(Situation, Now, At) :-
    ground(Situation),
    !,
    temporary_module(Situation, Module),
    assertz(Module:now(Now, At)).
now(Situation, Now, At) :-
    forall(situation_module(Situation, Module), assertz(Module:now(Now, At))).

canny:for_situation(fix, Situation) :-
    situation_module(Situation, Module),
    fix(Situation, Module).

fix(Situation, Module) :-
    findall(now(Now, At), retract(Module:now(Now, At)), Fixes0),
    sort(2, @=<, Fixes0, Fixes),
    forall(member(now(Was, When), Fixes), asserta(Module:was(Was, When))),
    ignore(retractall(Module:previously(_, _))),
    fix(Fixes, Situation, Module),
    broadcast(situation(Situation, fix)).

fix([], Situation, Module) :-
    once(retract(Module:currently(Previous, When))),
    !,
    asserta(Module:previously(Previous, When)),
    broadcast(situation(Situation, was(Previous, When))).
fix([], _, _) :-
    !.
fix(Fixes, Situation, Module) :-
    last(Fixes, now(Now, At)),
    fix(Situation, Module, Now, At).

fix(_, Module, Now, _) :-
    once(Module:currently(Now, _)),
    !.
fix(Situation, Module, Now, At) :-
    once(retract(Module:currently(Previous, When))),
    !,
    asserta(Module:previously(Previous, When)),
    asserta(Module:currently(Now, At)),
    broadcast(situation(Situation, was(Previous, When))),
    broadcast(situation(Situation, was(Previous, When), now(Now, At))),
    broadcast(situation(Situation, now(Now, At))).
fix(Situation, Module, Now, At) :-
    asserta(Module:currently(Now, At)),
    broadcast(situation(Situation, now(Now, At))).

canny:for_situation(retract(When), Situation) :-
    situation_module(Situation, Module),
    retract(Situation, Module, When).

retract(Situation, Module, When0) :-
    forall((   Module:was(Was, When),
               When < When0
           ), once(retract(Module:was(Was, When)))),
    broadcast(situation(Situation, retract(When0))).

canny:for_situation(retract(When, Delay), Situation) :-
    number(Delay),
    situation_module(Situation, Module),
    retract(Situation, Module, When, Delay).

retract(Situation, Module, When, Delay) :-
    var(When),
    !,
    once(Module:was(_, When)),
    retract(Situation, Module, When, Delay).
retract(Situation, Module, When, Delay) :-
    When0 is When - Delay,
    retract(Situation, Module, When0).

canny:for_situation(fix(Now, At), Situation) :-
    canny:for_situation(now(Now, At), Situation),
    canny:for_situation(fix, Situation).
canny:for_situation(fix(Now), Situation) :-
    canny:for_situation(now(Now), Situation),
    canny:for_situation(fix, Situation).
canny:for_situation(fixate(Now, Delay), Situation) :-
    get_time(At),
    canny:for_situation(fix(Now, At), Situation),
    canny:for_situation(retract(At, Delay), Situation).

canny:for_situation(listing, Situation) :-
    situation_module(Situation, Module),
    listing(Module:_).

%!  situation_property(?Situation:any, ?Property) is nondet.
%
%   Property of Situation.
%
%       * module(?Module)
%
%       Marries situation terms with universally-unique modules, one for
%       one. All dynamic situations link a situation term with a module.
%       This design addresses performance. Retracts   take  a long time,
%       relatively, especially for dynamic  predicates   with  very many
%       clauses; upwards of 10,000 clauses for   example.  Note, you can
%       never delete the  situation-module  association,   but  you  can
%       retract all the dynamic clauses belonging to a situation.
%
%       * defined
%
%       Situation is defined whenever a  unique situation module already
%       exists for the given Situation. Amounts   to  the same as asking
%       for module(_) property.

situation_property(Situation, Property) :-
    canny:property_of_situation(Property, Situation).

canny:property_of_situation(module(Module), Situation) :-
    situation_module(Situation, Module).
canny:property_of_situation(defined, Situation) :-
    situation_module(Situation, _).
canny:property_of_situation(currently(Current, When), Situation) :-
    situation_module(Situation, Module),
    once(Module:currently(Current, When)).
canny:property_of_situation(currently(Current), Situation) :-
    canny:property_of_situation(currently(Current, _), Situation).
canny:property_of_situation(previously(Previous, When), Situation) :-
    situation_module(Situation, Module),
    once(Module:previously(Previous, When)).
canny:property_of_situation(previously(Previous), Situation) :-
    canny:property_of_situation(previously(Previous, _), Situation).
canny:property_of_situation(history(History), Situation) :-
    situation_module(Situation, Module),
    findall(was(Was, When), Module:was(Was, When), History).
