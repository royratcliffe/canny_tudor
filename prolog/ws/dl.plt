:- begin_tests(ws_dl).

:- public test/1, test/2.

:- use_module(library(http/http_open)).
:- use_module(library(uri)).
:- use_module(dl).

test(dilbert) :-

%   Test requires Internet access. Applies XML namespace dialect and
%   space removal. The reply is a SOAP WSDL definitions element with
%   sub-elements carrying their namespace prefixes.

    url(URL),
    load_structure(URL, [Definitions], [dialect(xmlns), space(remove)]),
    xml_is_dom(Definitions),
    element(_:definitions, _, _) = Definitions,
    forall(element(Element), xpath(Definitions, _:Element, _)),
    forall(name_uri(Name, URI), element_xmlns(Definitions, Name=URI)),
    (   debugging(ws_dl)
    ->  xml_write(user_error, Definitions, [])
    ;   true
    ).

url('http://www.gcomputer.net/webservices/dilbert.asmx?WSDL').

element(types).
element(message).
element(portType).
element(binding).
element(service).

name_uri(tm,'http://microsoft.com/wsdl/mime/textMatching/').
name_uri(soapenc,'http://schemas.xmlsoap.org/soap/encoding/').
name_uri(mime,'http://schemas.xmlsoap.org/wsdl/mime/').
name_uri(tns,'http://gcomputer.net/webservices/').
name_uri(soap,'http://schemas.xmlsoap.org/wsdl/soap/').
name_uri(s,'http://www.w3.org/2001/XMLSchema').
name_uri(soap12,'http://schemas.xmlsoap.org/wsdl/soap12/').
name_uri(http,'http://schemas.xmlsoap.org/wsdl/http/').
name_uri(wsdl,'http://schemas.xmlsoap.org/wsdl/').

:- end_tests(ws_dl).
