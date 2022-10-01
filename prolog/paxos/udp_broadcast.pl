/*  File:    paxos/udp_broadcast.pl
    Author:  Roy Ratcliffe
    WWW:     https://github.com/royratcliffe
    Created: Jun 12 2021
    Purpose: Paxos UDP Broadcast

Copyright (c) 2021, Roy Ratcliffe, Northumberland, United Kingdom

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

:- module(paxos_udp_broadcast,
          [ paxos_udp_broadcast_initialise/0
          ]).
:- autoload(library(paxos), [paxos_initialize/1]).
:- autoload(library(udp_broadcast), [udp_broadcast_initialize/2]).
:- autoload(library(socket), [tcp_host_to_address/2]).
:- use_module(library(settings), [setting/4, setting/2]).

/** <module> Paxos on UDP

Sets up Paxos over UDP broadcast on port 20005. Hooks up Paxos messaging
to UDP broadcast bridging using the `paxos` scope.

Initialisation order affects success. First initialises UDP
broadcasting then initialises Paxos. The result is two additional
threads: the UDP inbound proxy and the Paxos replicator.

You can override the UDP host, port and broadcast scope. Load settings
first if you want to override using file-based settings. Back-up
defaults derive from the environment and finally fall on hard-wired
values of 0.0.0.0, port 20005 via `paxos` scope. You can also override
the automatic Paxos node ordinal; it defaults to -1 meaning automatic
discovering of unique node number. Numbers start at 0 and increase by
one, translating to binary power indices for the quorum bit mask.

Note that environment defaults require upper-case variable names for
Linux. Variable names match case-sensitively on Unix platforms.

---++ Docker Stack

For Docker in production mode, your nodes want to interact using the UDP
broadcast port. This port is not automatically available unless you
publish it. See example snippet below. The `ports` setting lists port
20005 for UDP broadcasts across the stack.

```
version: "3"

services:

  my-service:
    image: my/image
    ports:
      - 20005:20005/udp
      - 8080:8080/tcp
```

*/

:- setting(host, atom, env('PAXOS_UDP_BROADCAST_HOST', '0.0.0.0'),
           'UDP broadcast host for Paxos').
:- setting(port, nonneg, env('PAXOS_UDP_BROADCAST_PORT', 20005),
           'UDP broadcast port for Paxos').
:- setting(node, integer, env('PAXOS_UDP_BROADCAST_NODE', -1),
           'UDP broadcast node for Paxos').
:- setting(scope, atom, env('PAXOS_UDP_BROADCAST_SCOPE', paxos),
           'UDP broadcast scope for Paxos').

:- multifile paxos:paxos_message_hook/3.

paxos:paxos_message_hook(A, -, udp(Scope, A)) :- !, setting(scope, Scope).
paxos:paxos_message_hook(A, B, udp(Scope, A, B)) :- setting(scope, Scope).

paxos_udp_broadcast_initialise :-
    setting(host, Host),
    setting(port, Port),
    setting(scope, Scope),
    tcp_host_to_address(Host, Address),
    udp_broadcast_initialize(Address, [port(Port), scope(Scope)]),
    findall(PaxosOption, paxos_option(PaxosOption), PaxosOptions),
    paxos_initialize(PaxosOptions).

paxos_option(node(Node)) :- setting(node, Node), Node >= 0.
