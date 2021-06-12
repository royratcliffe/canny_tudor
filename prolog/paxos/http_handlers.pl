/*  File:    paxos/http_handlers.pl
    Author:  Roy Ratcliffe
    WWW:     https://github.com/royratcliffe
    Created: Jun 12 2021
    Purpose: Paxos HTTP Handlers

Copyright (c) 2021, Roy Ratcliffe, United Kingdom

Permission is hereby granted, free of charge,  to any person obtaining a
copy  of  this  software  and    associated   documentation  files  (the
"Software"), to deal in  the   Software  without  restriction, including
without limitation the rights to  use,   copy,  modify,  merge, publish,
distribute, sublicense, and/or sell  copies  of   the  Software,  and to
permit persons to whom the Software is   furnished  to do so, subject to
the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT  WARRANTY OF ANY KIND, EXPRESS
OR  IMPLIED,  INCLUDING  BUT  NOT   LIMITED    TO   THE   WARRANTIES  OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR   PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS  OR   COPYRIGHT  HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY,  WHETHER   IN  AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM,  OUT  OF   OR  IN  CONNECTION  WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/

:- module(paxos_http_handlers, []).
:- autoload(library(paxos),
            [ paxos_property/1,
              paxos_get/2,
              paxos_set/2
            ]).
:- autoload(library(http/http_dispatch), [http_handler/3]).
:- autoload(library(http/http_json),
            [ reply_json/1,
              reply_json/2,
              http_read_json/2
            ]).
:- use_module(library(swi/paxos)).

/** <module> Paxos HTTP Handlers

These handlers spool up a JSON-based HTTP interface to the Paxos
predicates, namely

    - paxos_property/1 as JSON object on GET at /paxos/properties,
    - paxos_get/2 as arbitrary JSON on GET at /paxos/Key and
    - paxos_set/2 as arbitrary JSON on POST at /paxos/Key

Take the example below. Uses http_server/1 to start a HTTP server
on some given port.

```prolog
?- [library(http/http_server), library(http/http_client)].
true.

?- http_server([port(8080)]).
% Started server at http://localhost:8080/
true.

?- http_get('http://localhost:8080/paxos/properties', A, []).
A = json([node=0, quorum=1, failed=0]).
```

Getting and setting using JSON encoding works as follows.

```prolog
?- http_get('http://localhost:8080/paxos/hello', A, [status_code(B)]).
A = '',
B = 204.

?- http_post('http://localhost:8080/paxos/hello', json(world), A, []).
A = @true.

?- http_get('http://localhost:8080/paxos/hello', A, [status_code(B)]).
A = world,
B = 200.
```

Note that the initial GET fails. It replies with the empty atom since no
content exists. Predicate paxos_get/2 is semi-deterministic; it can
fail. Empty atom is not valid Prolog-encoding for JSON. Status code of
204 indicates no content. The Paxos ledger does not contain data for
that key.

Thereafter, POST writes a string value for the key and a repeated GET
attempt now answers the new consensus data. Status code 200 indicates a
successful ledger concensus.

---++ Serialisation

Serialises unknowns. Paxos ledgers may contain non-JSON compatible data.
Anything that does not correctly serialise as JSON becomes an atomicly
rendered Prolog term. Take a consensus value of term a(1) for example;
GET requests see "a(1)" as a rendered Prolog string. The ledger
comprises Prolog terms, fundamentally, rather than JSON-encoded strings.

Setting a Paxos value reads JSON from the POST request body. It can be
any valid JSON value including atomic values as well as objects and
arrays.

*/

:- http_handler(root(paxos/properties), properties, []).
:- http_handler(root(paxos/Key), key(Method, Key),
                [ method(Method),
                  methods([get, post])
                ]).
:- http_handler(root(paxos/quorum), quorum, []).

%!  properties(+Request) is semidet.
%
%   Paxos properties request. The paxos_property/1 predicate answers
%   terms non-deterministically. Finds all the terms and relies on the
%   JSON serialiser to convert the node, quorum and failed terms to
%   correct JSON object key-value pairs. The JSON serialiser accepts
%   one-arity functors as pairs.

properties(_) :-
    findall(Property, paxos_property(Property), Properties),
    reply_json(json(Properties)).

%!  key(+Method, +Key, +Request) is semidet.
%
%   By design, the GET method reply represents failure as a no-content
%   status-204 response. This serves to disambiguate between a fail
%   response and a successful false response. JSON of false is a valid
%   quorum ledger data value, as in this example.
%
%   ```prolog
%   ?- http_post('http://localhost:8080/paxos/hello',
%                json(@false), A, [status_code(B)]).
%   A = @true,
%   B = 200.
%   ```
%
%   The POST method simply replies true or false in JSON when
%   paxos_set/2 succeeds or fails respectively.
%
%   Also note that Paxos gets and sets are **not** instantaneous.
%   Getting a key's value involves communication with the quorum since
%   the enquiring node does not necessarily carry the key at first.

key(get, Key, Request) :-
    request_options(Request, Options),
    (   paxos_get(Key, Data, Options)
    ->  reply_json(Data, [serialize_unknown(true)])
    ;   throw(http_reply(no_content))
    ).
key(post, Key, Request) :-
    http_read_json(Request, Data),
    request_options(Request, Options),
    (   paxos_set(Key, Data, Options)
    ->  Reply = true
    ;   Reply = false
    ),
    reply_json(@(Reply)).

%!  request_options(+Request, -Options) is det.
%
%   Finds HTTP Request parameters:
%
%       * reply(Replies:nonneg)
%       * timeout(Seconds:number)
%
%   Seconds must be in-between one millisecond and 10 seconds. These
%   arbitrary limits intend to strike a reasonable balance between
%   resource usages.

request_options(Request, Options) :-
    findall(Option, request_option(Request, Option), Options).

request_option(Request, retry(Retries)) :-
    http_parameters(Request,
                    [retry(Retries, [nonneg, optional(true)])]),
    nonvar(Retries).
request_option(Request, timeout(Seconds)) :-
    http_parameters(Request,
                    [ timeout(Seconds,
                              [ between(0.001, 10),
                                optional(true)
                              ])
                    ]),
    nonvar(Seconds).

quorum(_) :-
    paxos_property(node(Node)),
    paxos_quorum_nodes(Nodes),
    once(nth1(Nth1, Nodes, Node)),
    reply_json(json([nodes=Nodes, nth1=Nth1])).
