:- begin_tests(ws_dl).

:- public test/1, test/2.

:- use_module(library(http/http_open)).
:- use_module(dl).

test(dilbert) :-

%   Test requires Internet access. Applies XML namespace dialect and
%   space removal. The reply is a SOAP WSDL definitions element with
%   sub-elements carrying their namespace prefixes.

    url(URL),
    http_open(URL, Stream, []),
    load_structure(Stream, [Definitions], [dialect(xmlns), space(remove)]),
    xml_is_dom(Definitions),
    element(_:definitions, _, _) = Definitions,
    forall(element(Element), xpath(Definitions, _:Element, _)),
    debugging(wsdl) -> xml_write(user_error, Definitions, []).

url('http://www.gcomputer.net/webservices/dilbert.asmx?WSDL').

element(types).
element(message).
element(portType).
element(binding).
element(service).

:- end_tests(ws_dl).
