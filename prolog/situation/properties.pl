:- module(situation_properties, [situation_property/2]).

:- use_module(options, []).

:- meta_predicate situation_property(:, ?).

:- multifile canny:property_of_situation/2.

%!  situation_property(?Situation:compound, ?Property) is nondet.
%
%   Property of Situation.
%
%       * module(M)
%
%       Marries situation terms with universally-unique modules, one for
%       one. All dynamic situations link a situation term with a module.
%       This design addresses performance. Retracts   take  a long time,
%       relatively, especially for dynamic  predicates   with  very many
%       clauses; upwards of 10,000 clauses for   example.  Note, you can
%       never delete the  situation-module  association,   but  you  can
%       retract all the dynamic clauses belonging to a situation.

situation_property(Situation, Property) :-
    canny:property_of_situation(Property, Situation).

canny:property_of_situation(currently(Current, When), Situation) :-
    situation_property(Situation, module(M)),
    once(M:currently(Current, When)).
canny:property_of_situation(currently(Current), Situation) :-
    situation_property(Situation, currently(Current, _)).
canny:property_of_situation(previously(Previous, When), Situation) :-
    situation_property(Situation, module(M)),
    once(M:previously(Previous, When)).
canny:property_of_situation(previously(Previous), Situation) :-
    situation_property(Situation, previously(Previous, _)).
canny:property_of_situation(history(History), Situation) :-
    situation_property(Situation, module(M)),
    findall(Was-When, M:was(Was, When), History).
