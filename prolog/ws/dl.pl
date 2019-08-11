:- module(ws_dl, [element_xmlns/2]).

ns(wsdl, 'http://schemas.xmlsoap.org/wsdl/').

%!  element_xmlns(?Element, ?LocalURI) is nondet.
%
%   Extract =xmlns= attributes from the entire definitions DOM. They can
%   exist anywhere, not just in the root element. Any nested sub-element
%   can also carry XML namespace attribute pairs.

element_xmlns(element(_, Attributes, _), Local=URI) :-
    member(xmlns:Local=URI, Attributes).
element_xmlns(element(_, _, Content), Local=URI) :-
    member(Element, Content),
    element_xmlns(Element, Local=URI).
