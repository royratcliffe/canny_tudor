:- module(ws_dl,
          [   wsdl_load_definitions/2
          ]).

:- use_module(library(http/http_open), []).
:- use_module(library(uri), []).

:- dynamic definitions/2.

%!  wsdl_load_definitions(+Spec, -Definitions) is det.
%
%   Loads and caches WSDL definitions by Spec.   Fails  if the Spec does
%   not identify a  valid  XML  DOM   with  a  correct  definitions root
%   element.

wsdl_load_definitions(Spec, Definitions) :-
    definitions(Spec, Definitions),
    !.
wsdl_load_definitions(Spec, Definitions) :-
    load_structure(Spec, [Definitions], [dialect(xmlns), space(remove)]),
    xml_is_dom(Definitions),
    ns(wsdl, NS),
    element(NS:definitions, _, _) = Definitions,
    asserta(definitions(Spec, Definitions)).

ns(wsdl, 'http://schemas.xmlsoap.org/wsdl/').
