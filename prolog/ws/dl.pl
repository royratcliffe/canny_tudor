:- module(ws_dl, [element_xmlns/2, wsdl_qualified_name/3]).

ns(wsdl, 'http://schemas.xmlsoap.org/wsdl/').

%!  element_xmlns(+Element, ?NameURI) is nondet.
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

%!  wsdl_qualified_name(+WSDL:compound, ?QName:atom, ?URILocal:compound)
%   is nondet.
%
%   Unfies QName withits URI and Local part. Atom becomes compound, or
%   vice versa. WSDL definitions provide the context. Fails if the
%   context cannot match the Name. Relates a qualified name to a
%   URI:Local compound. Only Local needs to be ground; Name or URI, or
%   both, can be unbound.
%
%   QName is an atom. It is *not* a compound, even though it may look
%   like one. QName formats as an atom comprising a colon-delimited
%   Name and Local, where Name refers to a XML namespace qualifier and
%   Local refers to its arbitrary local part. There is no Name-based
%   unification. Unify Name=URI using element_xmlns/2 to narrow search
%   scope to some specific Name.
%
%   Tempting to prepend a second optimised clause for ground atomic
%   QName arguments.
%
%       wsdl_qualified_name(WSDL, QName, URI:Local) :-
%           atom(QName),
%           !,
%           sub_atom(QName, Before, _, After, :),
%           sub_atom(QName, 0, Before, _, Name),
%           element_xmlns(WSDL, Name=URI),
%           sub_atom(QName, _, After, 0, Local).

wsdl_qualified_name(WSDL, QName, URI:Local) :-
    element_xmlns(WSDL, Name=URI),
    atom_concat(Name, :, Name_),
    atom_concat(Name_, Local, QName).
