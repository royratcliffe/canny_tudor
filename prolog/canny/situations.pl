:- module(canny_situations, [situation_apply/2, situation_property/2]).

:- meta_predicate
    situation_apply(:, ?),              % ?Situation, ?Apply
    situation_property(:, ?).           % ?Situation, ?Property

:- multifile
    canny:apply_to_situation/2,
    canny:property_of_situation/2.

:- use_module(library(random/temporary)).

:- dynamic situation_module/2.

%!  situation_apply(?Situation:any, ?Apply) is nondet.
%
%   Mutates Situation. Apply term to Situation,   where  Apply is one of
%   the following. Note that the Apply  term   may  be nonground. It can
%   contain  variables  if  the   situation    mutation   generates  new
%   information.
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
%       * now(+Now:any)
%       * now(+Now:any, +At:number)
%
%       Makes some Situation become Now for time   index At, at the next
%       fixation. Effectively schedules a  pending   update  one or more
%       times; the next situation =fix/0=   fixes  the pending situation
%       changes at some future point. The   =now/1=  form applies Now to
%       Situation at the current Unix epoch time.
%
%       Uses canny:apply_to_situation/2 when Situation   is  ground, but
%       uses canny:property_of_situation/2 otherwise.  Asserts therefore
%       for multiple situations if Situation   comprises  variables. You
%       cannot therefore have non-ground situations.
%
%       * fix
%       * fix(+Now:any)
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
%       Term =fix/1= is a shortcut for now(Now, At) and =fix= where =At=
%       becomes the current Unix epoch time.  Fixes but does not retract
%       history terms.
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
%
%   The second argument Apply can be a list of terms to apply, including
%   nested lists of terms. All terms apply   in order first to last, and
%   depth first.
%
%   @arg Now is the state of a Situation  at some point in time. The Now
%   term must be non-variable but   not necessarily ground. Dictionaries
%   with unbound tags can exist within the situation calculus.

situation_apply(Situation, Apply) :- applies(Apply, Situation).

applies(Applies, Situation) :-
    is_list(Applies),
    !,
    member(Apply, Applies),
    applies(Apply, Situation).
applies(Apply, Situation) :- canny:apply_to_situation(Apply, Situation).

canny:apply_to_situation(module(Module), Situation) :-
    with_mutex(canny_situations, temporary_module(Situation, Module)).

temporary_module(Situation, Module) :- situation_module(Situation, Module), !.
temporary_module(Situation, Module) :-
    ground(Situation),
    once(random_temporary_module(Module)),
    asserta(situation_module(Situation, Module)),
    dynamic([   Module:now/2,
                Module:was/2,
                Module:currently/2,
                Module:previously/2
            ], []).

canny:apply_to_situation(now(Now, At), Situation) :-
    nonvar(Now),
    number(At),
    now(Situation, Now, At).
canny:apply_to_situation(now(Now), Situation) :-
    get_time(At),
    canny:apply_to_situation(now(Now, At), Situation).

now(Situation, Now, At) :-
    ground(Situation),
    !,
    temporary_module(Situation, Module),
    assertz(Module:now(Now, At)).
now(Situation, Now, At) :-
    forall(situation_module(Situation, Module), assertz(Module:now(Now, At))).

canny:apply_to_situation(fix, Situation) :-
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
fix([], _, _) :- !.
fix(Fixes, Situation, Module) :-
    last(Fixes, now(Now, At)),
    fix(Situation, Module, Now, At).

fix(_, Module, Now, _) :- once(Module:currently(Now, _)), !.
fix(Situation, Module, Now, At) :-
    once(retract(Module:currently(Previous, When))),
    !,
    asserta(Module:previously(Previous, When)),
    asserta(Module:currently(Now, At)),
    broadcast(situation(Situation, was(Previous, When), now(Now, At))).
fix(Situation, Module, Now, At) :-
    asserta(Module:currently(Now, At)),
    broadcast(situation(Situation, now(Now, At))).

canny:apply_to_situation(retract(When), Situation) :-
    situation_module(Situation, Module),
    retract(Situation, Module, When).

retract(Situation, Module, When0) :-
    forall((   Module:was(Was, When),
               When < When0
           ), once(retract(Module:was(Was, When)))),
    broadcast(situation(Situation, retract(When0))).

canny:apply_to_situation(retract(When, Delay), Situation) :-
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

canny:apply_to_situation(fix(Now, At), Situation) :-
    canny:apply_to_situation(now(Now, At), Situation),
    canny:apply_to_situation(fix, Situation).
canny:apply_to_situation(fix(Now), Situation) :-
    canny:apply_to_situation(now(Now), Situation),
    canny:apply_to_situation(fix, Situation).
canny:apply_to_situation(fixate(Now, Delay), Situation) :-
    get_time(At),
    canny:apply_to_situation(fix(Now, At), Situation),
    canny:apply_to_situation(retract(At, Delay), Situation).

canny:apply_to_situation(listing, Situation) :-
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
%
%       * currently(?Current:any)
%       * currently(?Current:any, ?When:number)
%       * currently(Current:any, for(Seconds:number))
%
%       Unifies with Current for Situation and When it happened. Unifies
%       with the one  and  only  Current   state  for  all  the matching
%       Situation terms. Unifies non-deterministically for all Situation
%       solutions, but semi-deterministically for   Current  state. Thus
%       allows for multiple matching  situations   but  only one Current
%       solution.
%
%       You can replace the When  term   with  for(Seconds)  in order to
%       measure elapsed interval since fixing Situation. Same applies to
%       previously/2 except that the current situation time stamp serves
%       as the baseline time, else defaults to the current time.
%
%       * previously(?Previous:any)
%       * previously(?Previous:any, ?When:number)
%       * previously(Previous:any, for(Seconds:number))
%
%       Finds  Previous  state  of    Situation,   non-deterministically
%       resolving zero or more matching  Situation   terms.  Fails if no
%       previous Situation condition.
%
%       * history(?History:list(compound))
%
%       Unifies  History  with  all  current    and  previous  situation
%       conditions, including their time stamps.   History is a sequence
%       of compounds of the  form  was(Was,   When)  where  Situation is
%       effectively a primitive condition coordinate,   Was is a sensing
%       outcome and When marks the moment that the outcome transpired.

situation_property(Situation, Property) :-
    canny:property_of_situation(Property, Situation).

canny:property_of_situation(module(Module), Situation) :-
    situation_module(Situation, Module).
canny:property_of_situation(defined, Situation) :-
    situation_module(Situation, _).
canny:property_of_situation(currently(Current, When), Situation) :-
    situation_module(Situation, Module),
    once(Module:currently(Current, When0)),
    currently_for(When, When0).
canny:property_of_situation(currently(Current), Situation) :-
    canny:property_of_situation(currently(Current, _), Situation).
canny:property_of_situation(previously(Previous, When), Situation) :-
    situation_module(Situation, Module),
    once(Module:previously(Previous, When0)),
    previously_for(When, When0, Situation).
canny:property_of_situation(previously(Previous), Situation) :-
    canny:property_of_situation(previously(Previous, _), Situation).
canny:property_of_situation(history(History), Situation) :-
    situation_module(Situation, Module),
    findall(was(Was, When), Module:was(Was, When), History).

%!  currently_for(?When, +When0) is det.
%
%   The predicate has three outcomes.  (1)   Unifies  When with When0 if
%   unbound. (2) If When unifies with  for(Seconds) then Seconds unifies
%   with the difference between the last When stamp and the current time
%   stamp. This assumes that the situation   time  carries the same time
%   scale as Epoch time.  This  is   not  always  necessarily  the case,
%   however. (3) By default, the outgoing When unifies with the incoming
%   When0.

currently_for(When, When0) :- var(When), !, When = When0.
currently_for(for(Seconds), When) :- !, get_time(At), Seconds is At - When.
currently_for(When, When).

%!  previously_for(?When, +When0, +Situation) is det.
%
%   Previously for(Seconds) compares the previous  When with the current
%   When,  assuming  Situation  answers   to  currently(Current,  When).
%   Situations may have a previous without a  current if fixed without a
%   Now term.

previously_for(When, When0, _Situation) :- var(When), !, When = When0.
previously_for(for(Seconds), When, Situation) :-
    !,
    currently_at(At, Situation),
    Seconds is At - When.
previously_for(When, When, _Situation).

currently_at(At, Situation) :-
    canny:property_of_situation(currently(_, At), Situation),
    !.
currently_at(At, _Situation) :- get_time(At).
