:- module(xml_ns, [xml_element_ns/2, xml_qualified_name/3, xml_element/3]).

:- use_module(library(xpath)).

%!  xml_element_ns(+Element:compound, ?NameURI) is nondet.
%
%   Extracts =xmlns= attributes from the   entire  definitions DOM. They
%   can exist anywhere,  not  just  in   the  root  element.  Any nested
%   sub-element can also carry XML namespace attribute pairs.
%
%   Goes without saying that there should be no redefinitions; although
%   matching duplicates may exist. Iterates all Name=URI attributes even
%   if not unique or not matching.

xml_element_ns(element(_, Attributes, _), Name=URI) :-
    member(xmlns:Name=URI, Attributes).
xml_element_ns(element(_, _, Content), Name=URI) :-
    member(Element, Content),
    xml_element_ns(Element, Name=URI).

%!  xml_qualified_name(+Element:compound, ?QName:atom,
%!  ?URILocal:compound) is nondet.
%
%   Unifies QName withits URI and Local part. Atom becomes compound, or
%   vice versa. XML elements provide the context. Fails if the
%   context cannot match the Name. Relates a qualified name to a
%   URI:Local compound. Only Local needs to be ground; Name or URI, or
%   both, can be unbound.
%
%   QName is an atom. It is *not* a compound, even though it may look
%   like one. QName formats as an atom comprising a colon-delimited
%   Name and Local, where Name refers to a XML namespace qualifier and
%   Local refers to its arbitrary local part. There is no Name-based
%   unification. Unify Name=URI using xml_element_ns/2 to narrow search
%   scope to some specific Name.
%
%   Tempting to prepend a second optimised clause for ground atomic
%   QName arguments.
%
%       xml_qualified_name(Element, QName, URI:Local) :-
%           atom(QName),
%           !,
%           sub_atom(QName, Before, _, After, :),
%           sub_atom(QName, 0, Before, _, Name),
%           xml_element_ns(Element, Name=URI),
%           sub_atom(QName, _, After, 0, Local).

xml_qualified_name(Element, QName, URI:Local) :-
    xml_element_ns(Element, Name=URI),
    atom_concat(Name, :, Name_),
    atom_concat(Name_, Local, QName).

%!  xml_element(+Element0, ?Path, -Element) is nondet.
%
%   Unifies with XML elements by Path. Only navigates the root element's
%   namespace. This includes all directly-accessible namespace elements,
%   but excludes any such elements not  directly accessible via the root
%   element or its root-namespaced sub-tree.
%
%   @arg Path specifies an element or   element path. An atom identifies
%   an element; a slash-delimited compound   identifies a nested element
%   path. The Path  can  include   unbound  attribute  selections,  e.g.
%   element(@name=Name) for binding with an element's name.

xml_element(Element0, Supers/Sub, Element) :-
    !,
    xml_element(Element0, Supers, Element_),
    xml_element(Element_, Sub, Element).
xml_element(Element0, Sub, Element) :-
    element(NS:_, _, _) = Element0,
    xpath(Element0, NS:Sub, Element).
