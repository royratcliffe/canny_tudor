:- module(ws_dl, [element_xmlns/2]).

ns(wsdl, 'http://schemas.xmlsoap.org/wsdl/').

%!  element_xmlns(?Element, ?NameURI) is nondet.
%
%   Extracts =xmlns= attributes from the   entire  definitions DOM. They
%   can exist anywhere,  not  just  in   the  root  element.  Any nested
%   sub-element can also carry XML namespace attribute pairs.
%
%   Goes without saying that there should be no redefinitions; although
%   matching duplicates may exist. Iterates all Name=URI attributes even
%   if not unique or not matching.

element_xmlns(element(_, Attributes, _), Name=URI) :-
    member(xmlns:Name=URI, Attributes).
element_xmlns(element(_, _, Content), Name=URI) :-
    member(Element, Content),
    element_xmlns(Element, Name=URI).
