/*  File:    canny/docker.pl
    Author:  Roy Ratcliffe
    Created: Jul 11 2025
    Purpose: Docker API

Copyright (c) 2025, Roy Ratcliffe, Northumberland, United Kingdom

Permission is hereby granted, free of charge,  to any person obtaining a
copy  of  this  software  and    associated   documentation  files  (the
"Software"), to deal in  the   Software  without  restriction, including
without limitation the rights to  use,   copy,  modify,  merge, publish,
distribute, sub-license, and/or sell copies  of   the  Software,  and to
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

:- module(canny_docker,
          [ docker/2,                           % +Ask, -Reply
            docker/3,                           % +Operation, -Reply, +Options
            docker_path_options/3               % ?Operation, -Path, -Options
          ]).
:- use_module(library(apply), [maplist/3, convlist/3]).
:- use_module(library(atom), [restyle_identifier/3]).
:- use_module(library(lists), [append/3, member/2]).
:- use_module(library(option), [select_option/4]).
:- use_module(library(http/http_client), [http_get/3]).
:- use_module(library(http/json), [json_read_dict/2]).
:- use_module(library(settings), [setting/4, setting/2]).
:- use_module(placeholders).

:- setting(daemon_url, list, [ protocol(tcp),
                               host(localhost),
                               port(2375)
                             ], 'URL of Docker API').
:- setting(api_version, atom, 'v1.49', 'Version of Docker API').

/** <module> Canny Docker

This module provides an interface to the Docker API, allowing interaction with
Docker services through HTTP requests. It defines settings for the Docker daemon
URL and API version, and provides a predicate to construct URLs and options for
various Docker operations.

It supports operations such as listing containers, creating containers, and
checking the Docker system status. The module uses Prolog dictionaries to
represent JSON data structures, making it easy to work with the Docker API's
responses. It also includes utility predicates for transforming dictionary
key-value pairs and constructing paths for API requests. It is designed to be
used in conjunction with the HTTP client library to make requests to the Docker
API. It provides a flexible way to interact with Docker services, allowing for
dynamic construction of API requests based on the specified operations and
options.

## Docker API Operations

The module supports various Docker API operations, such as:

    - `system_ping`: Check if the Docker daemon is reachable.
    - `container_list`: List all containers.
    - `container_create`: Create a new container.
    - `network_create`: Create a new network.
    - `network_delete`: Delete a network.

These operations are defined in the Docker API specification and can be accessed
through the `docker/3` predicate, which constructs the appropriate URL and
options based on the operation and the settings defined in this module.

### Example container operations

The following examples demonstrate how to list and create Docker containers
using the `docker/3` predicate. The first example lists all containers, and the
second example creates a new container with a specified image and labels.
```prolog
?- docker(container_list, Reply).
Reply = [json(['Id'='abc123', 'Image'='ubuntu:latest', ...|...])].
?- docker(container_create, Reply, [post(json(json(['Image'=ubuntu,
   'Labels'=json(['Hello'=world)])))])).
Reply = _{Id:"abc123", Warnings:[]}.
```
The `container_list/2` predicate retrieves a list of all containers, returning
a list of dictionaries representing each container. Each dictionary contains
information such as the container ID, image, and other metadata.
The `container_create/3` predicate creates a new container with the specified
image and labels. The labels are specified as a JSON object, allowing for
flexible tagging of containers with metadata. The reply contains the ID of the
created container and any warnings that may have occurred during the creation
process. The labels can be used to organise and manage containers based on
specific criteria, such as purpose or owner.

### Example network operations

The following examples demonstrate how to create and delete a Docker network
using the `docker/3` predicate. The network is created with a name and labels,
and then deleted by its name.

```prolog
?- docker(network_create(_{name:my_network, labels:_{'my.label':'my-value'}}), A).
A = _{id:"1be0f5d2337ff6a6db79a59707049c199268591f49e3c9054fc698fe7916f9c3", warning:""}.

38 ?- docker(network_delete(my_network), A).
A = ''.
```

Note that the `network_create/2` predicate constructs a network with the
specified name and labels, and returns a reply containing the network ID and any
warnings. The `network_delete/2` predicate deletes the network by its name,
returning an empty reply if successful.

Labels can be used to tag networks with metadata, which can be useful
for organising and managing Docker resources. The labels are specified
as a dictionary with key-value pairs, where the keys and values are
strings. The labels are included in the network configuration when
creating a network, allowing for flexible and dynamic tagging of Docker
resources.

Labels can be used to filter and query networks, making it easier to
manage Docker resources based on specific criteria. For example, you can
create a network with a label indicating its purpose or owner, and then
use that label to find networks that match certain criteria. This allows
for more organised and efficient management of Docker resources,
especially in larger deployments with many networks and containers.

### Restyling Keys

The `docker/3` predicate transforms the keys in the input dictionary to
CamelCase format using the `restyle_key/3` predicate, which applies the
Docker-specific CamelCase naming convention to the keys. This
transformation is useful for ensuring that the keys in the input
dictionary match the expected format for the Docker API, making it
easier to work with the API and ensuring compatibility with the expected
request format.

The transformation is applied recursively to all
key-value pairs in the input dictionary, ensuring that all keys are
transformed to the correct format before making the request to the
Docker API. The reverse transformation is applied to the reply
dictionary, which does not retain the original key names as returned by
the Docker API. Label keys are also transformed to CamelCase format,
ensuring consistency in the naming convention used for labels in the
Docker API requests and responses.

## Low-Level HTTP Requests

The module provides a low-level interface to the Docker API, allowing for custom
HTTP requests to be made. The `docker/3` predicate constructs the URL and
options for the specified operation, and uses the `http_get/3` predicate to make
the request. The options can include HTTP methods, headers, and other parameters
as needed for the specific operation.

The `url_options/4` predicate is used to construct the URL and options for a
specific Docker operation. It retrieves the operation details from the Docker
API specification and formats the path according to the specified version and
operation. The resulting URL and options can be used with the HTTP client to
make requests to the Docker API.

### Example usage

The `url_options/4` predicate can be used to construct the URL and options for a
specific Docker operation. For example, to get the URL and options for the
`system_ping` operation, you can use:

```prolog
?- [library(http/http_client)].
true.

?- canny_docker:url_options(system_ping, URL, Options),
   http_get(URL, Reply, Options).
URL = [path('/v1.49/_ping'), protocol(tcp), host(localhost), port(2375)],
Options = [method(get), accept(["text/plain"])],
Reply = 'OK'.
```

For listing containers, you can use:

```prolog
?- canny_docker:url_options(container_list, URL, Options),
   http_get(URL, Reply, Options).
URL = [path('/v1.49/containers/json'), protocol(tcp), host(localhost), port(2375)],
Options = [method(get), accept(["application/json"])],
Reply = [json(['Id'=..., ...|...])].
```

For creating a container, you can use:

```prolog
?- docker(container_create, A, [post(json(json(['Image'=ubuntu,
   'Labels'=json(['Hello'=world])])))]).
```

This example creates a new Docker container with the specified image and labels.
Notice that the post request uses `json(json(...))` to specify the JSON body of
the request.

@author Roy Ratcliffe
@version 0.1.0
*/

%!  docker(+Ask, -Reply) is det.
%
%   Issues a request to the Docker API  using the specified Ask term and
%   returns the Reply. The Ask term  may   be  a compound specifying the
%   operation to perform together with any required arguments.
%
%   The Docker API request comprises:
%   - a path with zero or more placeholders,
%   - a method,
%   - zero or more required or optional search parameters,
%   - a JSON body for POST requests.
%   This implies that, for the least amount of additional information, a
%   request is just a path with a method, e.g., a GET, HEAD or DELETE
%   request. From that point onward, requests grow in complexity
%   involving or more of the following: path placeholders, query
%   parameters, a request body.

docker(Ask, Reply) :-
    Ask =.. [Functor|Arguments],
    ask(Arguments, Functor, URL_, Options),
    setting(daemon_url, URL0),
    append(URL_, URL0, URL),
    http_get(URL, Reply0, [json_object(dict)|Options]),
    restyle_value(one_two, Reply0, Reply).

ask([], Functor, [path(Path)], Options) :-
    ask(Functor, [Path], [], _, Options).
ask([Value], Functor, [path(Path)], Options) :-
    atomic(Value),
    ask(Functor, Terms, [Placeholder], _, Options),
    !,
    Placeholder =.. [_, Value],
    atomic_list_concat(Terms, '', Path).
ask([Queries], Functor, [path(Path), search(Searches)], Options) :-
    is_list(Queries),
    ask(Functor, [Path], [], Queries0, Options),
    !,
    convlist(query_search(Queries0), Queries, Searches).
ask([Dict], Functor, [path(Path)], [post(json(Dict))|Options]) :-
    is_dict(Dict),
    ask(Functor, Terms, [], _, Options),
    !,
    option(method(post), Options),
    atomic_list_concat(Terms, '', Path).
ask([Value, Queries], Functor, [path(Path), search(Searches)], Options) :-
    atomic(Value),
    is_list(Queries),
    ask(Functor, Terms, [Placeholder], Queries0, Options),
    !,
    Placeholder =.. [_, Value],
    atomic_list_concat(Terms, '', Path),
    convlist(query_search(Queries0), Queries, Searches).
ask([Value, Dict], Functor, [path(Path)], [post(json(Dict))|Options]) :-
    atomic(Value),
    is_dict(Dict),
    ask(Functor, Terms, [Placeholder], _, Options),
    !,
    % The cut is not strictly necessary, but it ensures that
    % no further clauses are considered, should any be added in future.
    %
    % Placeholder is a one-arity functor that will be unified with
    % the Value argument. The placeholder is used to construct the
    % path, and the Value is the argument that replaces the placeholder.
    Placeholder =.. [_, Value],
    atomic_list_concat(Terms, '', Path),
    convlist(query_search(Queries0), Queries, Searches).

query_search(Queries, Query, Search) :-
    Query =.. [Name, _],
    Search =.. [Name, _],
    option(Search, Queries).

%!  ask(+Operation, ?Terms, ?Placeholders, ?Queries, -Options) is semidet.
%
%   Constructs a Docker API request based on the specified operation, terms,
%   placeholders, queries, and options. The operation is an atom that identifies
%   the Docker API operation to perform, such as `container_list` or
%   `system_ping`. The Terms are a list of terms that represent the atom spans
%   and placeholders in the operation's path. The Placeholders are a list of
%   one-arity functors that will be unified in the path Terms with their
%   corresponding arguments. The Queries are a list of terms that represent
%   additional search parameters for the request. The Options are a list of
%   terms that control how the HTTP request is made.

ask(Operation, Terms, Placeholders, Queries, Options) :-
    docker_path_options(Operation, Path, Options0),
    select_option(query(Queries), Options0, Options),
    atom_codes(Path, Codes),
    phrase(placeholders([], Terms, [], Placeholders), Codes).

%!  mapdict(+Goal, +Dict0, -Dict) is det.
%
%   Applies a goal to each key-value  pair in a dictionary, transforming
%   the pairs according to the goal. The   goal is typically a predicate
%   that  processes  each   key-value   pair,    allowing   for   custom
%   transformations. The predicate takes an input dictionary `Dict0` and
%   applies the goal to each key-value  pair, producing a new dictionary
%   `Dict` with the transformed pairs.
%
%   The goal is applied to each key-value  pair in the input dictionary,
%   and the results are collected into a   new  dictionary. The keys and
%   values may be transformed according to the  goal. This is useful for
%   applying transformations to dictionary entries, such as changing the
%   style of keys or values, or filtering   out certain entries based on
%   specific criteria.
%
%   The predicate uses `dict_pairs/3` to convert   the dictionary into a
%   list of key-value  pairs,  applies  the   goal  to  each  pair using
%   `maplist/3`, and then converts the list   of  transformed pairs back
%   into a dictionary  using  `dict_pairs/3`   again.  This  allows  for
%   flexible and efficient processing  of   dictionary  entries  without
%   needing to manually iterate over the keys and values.
%
%   @param Goal The goal  to  apply  to   each  key-value  pair  in  the
%   dictionary. The goal should be a   predicate  that takes a key-value
%   pair and produces a transformed  key-value   pair.  The  goal can be
%   defined as a predicate  that  takes   two  arguments:  the  original
%   key-value pair and the transformed key-value pair.
%
%   @param Dict0 The input dictionary containing  the key-value pairs to
%   be  transformed.  The  dictionary  is  expected    to  be  a  Prolog
%   dictionary, which is a key-value  store   where  keys  are atoms and
%   values can be any Prolog term.
%
%   @param  Dict  The  output  dictionary   containing  the  transformed
%   key-value pairs. The keys in this dictionary  are the same as in the
%   input dictionary, but the values may have been transformed according
%   to the specified goal.

:- meta_predicate mapdict(2, +, -).

mapdict(Goal, Dict0, Dict) :-
    dict_pairs(Dict0, _, Pairs0),
    maplist(Goal, Pairs0, Pairs),
    dict_pairs(Dict, _, Pairs).

%!  restyle_key(+Style, +Pair0, -Pair) is det.
%
%   Restyles a key-value pair in a dictionary according to the specified
%   style. The key  is  transformed   using  the  `restyle_identifier/3`
%   predicate, which applies a specific naming   convention  to the key.
%   The value is processed recursively if   it is a dictionary, ensuring
%   that all nested key-value pairs are also restyled.
%
%   The predicate takes a style identifier and  a key-value pair, and it
%   returns a new key-value pair with   the key transformed according to
%   the specified style. If the value   is  a dictionary, it recursively
%   applies the same transformation to all   key-value pairs within that
%   dictionary. The transformation  is  useful   for  adapting  keys  to
%   different naming conventions, such as  converting from snake_case to
%   camelCase or vice versa.
%
%   @param Style The style identifier  that   determines  how the key is
%   transformed.  For  example,  'OneTwo'  might    convert   keys  from
%   snake_case to CamelCase.
%
%   @param Pair0 The original key-value pair, where   the key is an atom
%   and the value can be any Prolog term, including another dictionary.
%
%   @param Pair The transformed key-value pair,   where the key has been
%   restyled according to the specified style,   and the value is either
%   unchanged or recursively transformed if it is a dictionary.

restyle_key(Style, Key0-Value0, Key-Value) :-
    % What if the value is a list? And what if the list contains dictionaries?
    % Should we apply the restyling to each element? What if the list contains
    % sub-lists? Should we apply the restyling to each element of the sub-lists?
    %
    % If the value is a list, apply restyle_value recursively to each element.
    % This handles lists of dictionaries and nested lists.
    restyle_identifier(Style, Key0, Key),
    restyle_value(Style, Value0, Value).

restyle_value(Style, Value0, Value) :-
    (   is_dict(Value0)
    ->  mapdict(restyle_key(Style), Value0, Value)
    ;   is_list(Value0)
    ->  maplist(restyle_value(Style), Value0, Value)
    ;   Value = Value0
    ).

%!  docker(+Operation, -Reply, +Options) is det.
%
%   Makes a request to the Docker API using the specified operation and
%   options. The operation is a string that identifies the Docker API
%   operation to perform, such as `container_list` or `system_ping`. The
%   predicate constructs the URL and options based on the operation and
%   the settings defined in this module.
%
%   Builds HTTP request options for the Docker API using the base URL from
%   the `daemon_url` setting. The path and HTTP method are determined by
%   `path_and_method/4`, and the resulting options are suitable for making
%   requests to the Docker API.
%
%   The predicate constructs the URL by concatenating the base URL with
%   the path and method. The `daemon_url` setting provides the base URL,
%   and the `api_version` setting specifies the version of the Docker API.
%
%   @param Operation The operation to perform, which determines the path and
%   method, as well as any additional options.
%   @param Reply The response from the Docker API, which is typically a
%   Prolog dictionary or list, depending on the operation.
%   @param Options This is a list of options that control both how the path is
%   formatted and how the HTTP request is made. For path formatting, options are
%   terms like `id(Value)` that provide values for placeholders in the path
%   template. For the HTTP request, options can include settings such as
%   headers, authentication, or other parameters supported by the HTTP client.

docker(Operation, Reply, Options) :-
    setting(daemon_url, URL),
    setting(api_version, Version),
    docker_path_options(Version, Operation, Path_, Options_),
    format_path(Path_, Path, Options),
    select_option(search(Search), Options, Options0, []),
    append(Options0, Options_, Options__),
    http_get([path(Path), search(Search)|URL], Reply, Options__).

%!  format_path(+Format:atom, -Path:atom, +Options:list)// is semidet.
%
%   Constructs a path by replacing  placeholders   in  the format string
%   with values from the options  list.   Placeholders  are specified as
%   `{name}` and are substituted with the corresponding value for `name`
%   found in Options. The final  path   is  formed  by concatenating all
%   components after substitution.
%
%   The format string can  contain  any   number  of  placeholders, each
%   enclosed in curly braces, such as   `{name}`.  When constructing the
%   path, the predicate scans the  format   string  from  left to right,
%   replacing each placeholder  with  the   value  associated  with  the
%   corresponding key in the options list.  The options list is expected
%   to contain terms of the form `name(Value)`, where `name` matches the
%   placeholder. All non-placeholder  text  in   the  format  string  is
%   preserved as-is.
%
%   Placeholders are substituted in the order  they appear in the format
%   string, allowing for flexible and dynamic   path  construction. If a
%   placeholder is found in the format string but no corresponding value
%   exists in the options list, the   predicate fails, ensuring that all
%   required values are provided.
%
%   This mechanism enables the dynamic generation   of API paths or file
%   paths without hardcoding  specific  values,   making  the  code more
%   maintainable and adaptable to changes   in  configuration or runtime
%   parameters.    For    example,     given      a     format    string
%   `'/containers/{id}/json'`   and   options    `[id('abc123')]`,   the
%   resulting path would be `'/containers/abc123/json'`.
%
%   @param Format The format string containing placeholders for options.
%   @param Path The resulting formatted path as an atom.
%   @param Options List of options to be used for formatting the path.

format_path(Format, Path, Options) :-
    format_placeholders(Format, Path, Options).

                /*******************************
                *          DOCKER API          *
                *******************************/

%!  docker_path_options(?Operation, -Path, -Options) is semidet.
%
%   Constructs the Path and Options for a Docker API operation. The predicate
%   retrieves the operation details from the Docker API specification and
%   formats the path according to the default version and operation. The
%   resulting path and options can be used with the HTTP client to make requests
%   to the Docker API.
%
%   The predicate uses the `docker_path_options/4` predicate to construct the
%   path and options for the specified operation. It retrieves the operation
%   details from the Docker API specification and formats the path according to
%   the specified version and operation. The resulting path and options can be
%   used with the HTTP client to make requests to the Docker API.
%
%   | build_prune | '/v1.49/build/prune' | [method(post),accept(["application/json"])] |
%   | config_create | '/v1.49/configs/create' | [method(post),accept(["application/json"])] |
%   | config_delete | '/v1.49/configs/{id}' | [method(delete),accept(["application/json"])] |
%   | config_inspect | '/v1.49/configs/{id}' | [method(get),accept(["application/json"])] |
%   | config_list | '/v1.49/configs' | [method(get),accept(["application/json"])] |
%   | config_update | '/v1.49/configs/{id}/update' | [method(post)] |
%   | container_archive | '/v1.49/containers/{id}/archive' | [method(get),accept(["application/x-tar"])] |
%   | container_archive_info | '/v1.49/containers/{id}/archive' | [method(head)] |
%   | container_attach | '/v1.49/containers/{id}/attach' | [method(post),accept(["application/vnd.docker.raw-stream","application/vnd.docker.multiplexed-stream"])] |
%   | container_attach_websocket | '/v1.49/containers/{id}/attach/ws' | [method(get)] |
%   | container_changes | '/v1.49/containers/{id}/changes' | [method(get),accept(["application/json"])] |
%   | container_create | '/v1.49/containers/create' | [method(post),accept(["application/json"])] |
%   | container_delete | '/v1.49/containers/{id}' | [method(delete)] |
%   | container_exec | '/v1.49/containers/{id}/exec' | [method(post),accept(["application/json"])] |
%   | container_export | '/v1.49/containers/{id}/export' | [method(get),accept(["application/octet-stream"])] |
%   | container_inspect | '/v1.49/containers/{id}/json' | [method(get),accept(["application/json"])] |
%   | container_kill | '/v1.49/containers/{id}/kill' | [method(post)] |
%   | container_list | '/v1.49/containers/json' | [method(get),accept(["application/json"])] |
%   | container_logs | '/v1.49/containers/{id}/logs' | [method(get),accept(["application/vnd.docker.raw-stream","application/vnd.docker.multiplexed-stream"])] |
%   | container_pause | '/v1.49/containers/{id}/pause' | [method(post)] |
%   | container_prune | '/v1.49/containers/prune' | [method(post),accept(["application/json"])] |
%   | container_rename | '/v1.49/containers/{id}/rename' | [method(post)] |
%   | container_resize | '/v1.49/containers/{id}/resize' | [method(post),accept(["text/plain"])] |
%   | container_restart | '/v1.49/containers/{id}/restart' | [method(post)] |
%   | container_start | '/v1.49/containers/{id}/start' | [method(post)] |
%   | container_stats | '/v1.49/containers/{id}/stats' | [method(get),accept(["application/json"])] |
%   | container_stop | '/v1.49/containers/{id}/stop' | [method(post)] |
%   | container_top | '/v1.49/containers/{id}/top' | [method(get)] |
%   | container_unpause | '/v1.49/containers/{id}/unpause' | [method(post)] |
%   | container_update | '/v1.49/containers/{id}/update' | [method(post),accept(["application/json"])] |
%   | container_wait | '/v1.49/containers/{id}/wait' | [method(post),accept(["application/json"])] |
%   | distribution_inspect | '/v1.49/distribution/{name}/json' | [method(get),accept(["application/json"])] |
%   | exec_inspect | '/v1.49/exec/{id}/json' | [method(get),accept(["application/json"])] |
%   | exec_resize | '/v1.49/exec/{id}/resize' | [method(post)] |
%   | exec_start | '/v1.49/exec/{id}/start' | [method(post),accept(["application/vnd.docker.raw-stream","application/vnd.docker.multiplexed-stream"])] |
%   | get_plugin_privileges | '/v1.49/plugins/privileges' | [method(get)] |
%   | image_build | '/v1.49/build' | [method(post),accept(["application/json"])] |
%   | image_commit | '/v1.49/commit' | [method(post),accept(["application/json"])] |
%   | image_create | '/v1.49/images/create' | [method(post),accept(["application/json"])] |
%   | image_delete | '/v1.49/images/{name}' | [method(delete),accept(["application/json"])] |
%   | image_get | '/v1.49/images/{name}/get' | [method(get),accept(["application/x-tar"])] |
%   | image_get_all | '/v1.49/images/get' | [method(get),accept(["application/x-tar"])] |
%   | image_history | '/v1.49/images/{name}/history' | [method(get),accept(["application/json"])] |
%   | image_inspect | '/v1.49/images/{name}/json' | [method(get),accept(["application/json"])] |
%   | image_list | '/v1.49/images/json' | [method(get),accept(["application/json"])] |
%   | image_load | '/v1.49/images/load' | [method(post),accept(["application/json"])] |
%   | image_prune | '/v1.49/images/prune' | [method(post),accept(["application/json"])] |
%   | image_push | '/v1.49/images/{name}/push' | [method(post)] |
%   | image_search | '/v1.49/images/search' | [method(get),accept(["application/json"])] |
%   | image_tag | '/v1.49/images/{name}/tag' | [method(post)] |
%   | network_connect | '/v1.49/networks/{id}/connect' | [method(post)] |
%   | network_create | '/v1.49/networks/create' | [method(post),accept(["application/json"])] |
%   | network_delete | '/v1.49/networks/{id}' | [method(delete)] |
%   | network_disconnect | '/v1.49/networks/{id}/disconnect' | [method(post)] |
%   | network_inspect | '/v1.49/networks/{id}' | [method(get),accept(["application/json"])] |
%   | network_list | '/v1.49/networks' | [method(get),accept(["application/json"])] |
%   | network_prune | '/v1.49/networks/prune' | [method(post),accept(["application/json"])] |
%   | node_delete | '/v1.49/nodes/{id}' | [method(delete)] |
%   | node_inspect | '/v1.49/nodes/{id}' | [method(get)] |
%   | node_list | '/v1.49/nodes' | [method(get)] |
%   | node_update | '/v1.49/nodes/{id}/update' | [method(post)] |
%   | plugin_create | '/v1.49/plugins/create' | [method(post)] |
%   | plugin_delete | '/v1.49/plugins/{name}' | [method(delete)] |
%   | plugin_disable | '/v1.49/plugins/{name}/disable' | [method(post)] |
%   | plugin_enable | '/v1.49/plugins/{name}/enable' | [method(post)] |
%   | plugin_inspect | '/v1.49/plugins/{name}/json' | [method(get)] |
%   | plugin_list | '/v1.49/plugins' | [method(get),accept(["application/json"])] |
%   | plugin_pull | '/v1.49/plugins/pull' | [method(post),accept(["application/json"])] |
%   | plugin_push | '/v1.49/plugins/{name}/push' | [method(post)] |
%   | plugin_set | '/v1.49/plugins/{name}/set' | [method(post)] |
%   | plugin_upgrade | '/v1.49/plugins/{name}/upgrade' | [method(post)] |
%   | put_container_archive | '/v1.49/containers/{id}/archive' | [method(put)] |
%   | secret_create | '/v1.49/secrets/create' | [method(post),accept(["application/json"])] |
%   | secret_delete | '/v1.49/secrets/{id}' | [method(delete),accept(["application/json"])] |
%   | secret_inspect | '/v1.49/secrets/{id}' | [method(get),accept(["application/json"])] |
%   | secret_list | '/v1.49/secrets' | [method(get),accept(["application/json"])] |
%   | secret_update | '/v1.49/secrets/{id}/update' | [method(post)] |
%   | service_create | '/v1.49/services/create' | [method(post),accept(["application/json"])] |
%   | service_delete | '/v1.49/services/{id}' | [method(delete)] |
%   | service_inspect | '/v1.49/services/{id}' | [method(get)] |
%   | service_list | '/v1.49/services' | [method(get)] |
%   | service_logs | '/v1.49/services/{id}/logs' | [method(get),accept(["application/vnd.docker.raw-stream","application/vnd.docker.multiplexed-stream"])] |
%   | service_update | '/v1.49/services/{id}/update' | [method(post),accept(["application/json"])] |
%   | session | '/v1.49/session' | [method(post),accept(["application/vnd.docker.raw-stream"])] |
%   | swarm_init | '/v1.49/swarm/init' | [method(post),accept(["application/json","text/plain"])] |
%   | swarm_inspect | '/v1.49/swarm' | [method(get)] |
%   | swarm_join | '/v1.49/swarm/join' | [method(post)] |
%   | swarm_leave | '/v1.49/swarm/leave' | [method(post)] |
%   | swarm_unlock | '/v1.49/swarm/unlock' | [method(post),accept(["application/json"])] |
%   | swarm_unlockkey | '/v1.49/swarm/unlockkey' | [method(get)] |
%   | swarm_update | '/v1.49/swarm/update' | [method(post)] |
%   | system_auth | '/v1.49/auth' | [method(post),accept(["application/json"])] |
%   | system_data_usage | '/v1.49/system/df' | [method(get)] |
%   | system_events | '/v1.49/events' | [method(get),accept(["application/json"])] |
%   | system_info | '/v1.49/info' | [method(get),accept(["application/json"])] |
%   | system_ping | '/v1.49/_ping' | [method(get),accept(["text/plain"])] |
%   | system_ping_head | '/v1.49/_ping' | [method(head),accept(["text/plain"])] |
%   | system_version | '/v1.49/version' | [method(get),accept(["application/json"])] |
%   | task_inspect | '/v1.49/tasks/{id}' | [method(get),accept(["application/json"])] |
%   | task_list | '/v1.49/tasks' | [method(get),accept(["application/json"])] |
%   | task_logs | '/v1.49/tasks/{id}/logs' | [method(get),accept(["application/vnd.docker.raw-stream","application/vnd.docker.multiplexed-stream"])] |
%   | volume_create | '/v1.49/volumes/create' | [method(post),accept(["application/json"])] |
%   | volume_delete | '/v1.49/volumes/{name}' | [method(delete)] |
%   | volume_inspect | '/v1.49/volumes/{name}' | [method(get),accept(["application/json"])] |
%   | volume_list | '/v1.49/volumes' | [method(get),accept(["application/json"])] |
%   | volume_prune | '/v1.49/volumes/prune' | [method(post),accept(["application/json"])] |
%   | volume_update | '/v1.49/volumes/{name}' | [method(put),accept(["application/json"])] |
%
%   @param Operation The operation to perform, which determines the path and
%   method, as well as any additional options.
%   @param Path The path for the operation, which is derived from the
%   Docker API specification.
%   @param Options List of options for the HTTP request, such as `method` and
%   `accept`.

docker_path_options(Operation, Path, Options) :-
    setting(api_version, Version),
    docker_path_options(Version, Operation, Path, Options).

%!  docker_path_options(+Version, ?Operation, -Path, -Options) is semidet.
%
%   The predicate succeeds if the given operation is present in the
%   `load_docker_api_json/2` dictionary. The dictionary is read from a JSON file
%   that contains the Docker API specification.
%
%   @param Version The version of the Docker API to read.
%   @param Operation The operation to perform, which determines the path and
%   method, as well as any additional options.
%   @param Path The path for the operation, which is derived from the
%   Docker API specification.
%   @param Options List of options for the HTTP request.

docker_path_options(Version, Operation, Path, [method(Method)|Options]) :-
    % Look up the operation in the Docker API specification. Fail if not found.
    % Support variable Operation for dynamic queries.
    (   var(Operation)
    ->  operation(Version, Operation, Path_, Method, Options)
    ;   once(operation(Version, Operation, Path_, Method, Options))
    ),
    atom_concat(/, Version, Path0),
    atom_concat(Path0, Path_, Path).

%!  operation(+Version, ?Operation, -Path, -Method, -Options) is nondet.
%
%   Retrieves the operation, path, and method from the Docker API JSON
%   specification. The predicate uses the `load_docker_api_json/2` predicate to
%   read the Docker API specification and extract the operation details.
%
%   The predicate succeeds if the given operation is present in the
%   `load_docker_api_json/2` dictionary. The dictionary is read from a JSON file
%   that contains the Docker API specification.
%
%   @param Version The version of the Docker API to read.
%   @param Operation The operation to perform, which determines the path and
%   method, as well as any additional options.
%   @param Path The path for the operation, which is derived from the
%   Docker API specification.
%   @param Method The HTTP method for the operation, such as `get`, `post`,
%   or `delete`.
%   @param Options List of options for the method, such as `accept` for
%   the expected response format.

% Tablise the operation/5 predicate to allow for efficient retrieval of
% operations based on the version, operation name, path, method, and options. It
% allows for quick lookups of operations without needing to repeatedly parse the
% Docker API JSON specification.
:- table operation/5.

operation(Version, Operation, Path, Method, Options) :-
    load_docker_api_json(Version, Dict),
    path_and_method(Dict.paths, Path, Method, MethodDict),
    get_dict(operationId, MethodDict, OperationId),
    restyle_identifier(one_two, OperationId, Operation),
    dict_pairs(MethodDict, _, Pairs),
    convlist(method_option, Pairs, Options).

method_option(produces-Produces, accept(Produces)).
method_option(parameters-Parameters, query(Terms)) :-
    convlist(query_parameter, Parameters, Terms).

%!  query_parameter(+Parameter, -Term) is semidet.
%
%   Converts a query parameter from the Docker API specification into a
%   Prolog term. The parameter is expected to be a dictionary with keys
%   `in`, `name`, and `type`. The predicate constructs a term of the form
%   `Name(Type)` where `Name` is the name of the parameter and `Type` is its type.

query_parameter(Parameter, Term) :-
    _{in:"query", name:Name, type:Type} :< Parameter,
    atom_string(Name_, Name),
    atom_string(Type_, Type),
    Term =.. [Name_, Type_].

%!  path_and_method(+Paths, -Path, -Method, -MethodDict) is nondet.
%
%   Retrieves a path and its corresponding   method from a dictionary of
%   paths. Succeeds if the given path  and   method  are  present in the
%   dictionary. The predicate extracts the  path   and  method  from the
%   dictionary  and  unifies  them  with  the  provided  variables.  The
%   predicate succeeds if the given path and   method are present in the
%   dictionary.
%
%   @param Paths Dictionary mapping paths to method dictionaries.
%   @param Path The extracted path key.
%   @param Method The extracted HTTP method.
%   @param MethodDict Dictionary with details for the extracted method.

path_and_method(Paths, Path, Method, MethodDict) :-
    dict_pairs(Paths, _, PathPairs),
    member(Path-PathDict, PathPairs),
    dict_pairs(PathDict, _, MethodPairs),
    member(Method-MethodDict, MethodPairs).

%!  load_docker_api_json(+Version, -Dict) is det.
%
%   Reads a Docker JSON file for  a   specific  version  and returns its
%   contents as a Prolog dictionary. The   predicate constructs the file
%   path based on the version and reads the JSON data from the file.
%
%   The predicate uses the `docker_api_json_path/2` predicate to resolve the
%   file path relative to the current module's source file directory.
%
%   @param Version The version of the Docker  API to read. The predicate
%   reads the JSON data from the  file   and  unifies it with the `Dict`
%   variable.  The  JSON  file  is  expected    to   be  in  the  format
%   =|<version>.json|=, where =|<version>|=  is   the  specified version
%   with its `v` prefix.
%
%   @param Dict The Prolog dictionary containing the JSON data read from
%   the Docker JSON file. The dictionary  contains the configuration and
%   metadata API for Docker.

load_docker_api_json(Version, Dict) :-
    docker_api_json_path(Version, Abs),
    setup_call_cleanup(open(Abs, read, In),
                       json_read_dict(In, Dict),
                       close(In)).

%!  docker_api_json_path(+Base, -Abs) is det.
%
%   Constructs the absolute path of a Docker   JSON file based on a base
%   name. The base name is expected to   have a =|.json|= extension. The
%   predicate uses the `context_file/3` predicate   to  resolve the file
%   path relative to the current module's source file directory.
%
%   The Docker JSON file stores the   configuration and metadata API for
%   Docker.  Provide  the  base  version   name  without  the  =|.json|=
%   extension; the predicate automatically  appends   it.  The predicate
%   unifies the absolute path to the term at `Abs`. The `context_file/3`
%   predicate is used to resolve the file   path relative to the current
%   module's source file directory.
%
%   @param Base The base name of  the   Docker  JSON file, excluding the
%   =|.json|= extension. This corresponds  to   the  Docker API version,
%   prefixed with `v`.
%
%   @param Abs The absolute file path of the Docker JSON file with the
%   =|.json|= extension.

docker_api_json_path(Base, Abs) :-
    file_name_extension(Base, json, Name),
    context_file((..)/docker/Name, Abs, [access(exist)]).

                /*******************************
                *           CONTEXT            *
                *******************************/

%!  context_file(+Spec, -Abs, +Options) is det.
%
%   Determines the absolute path of a file Spec, resolving it relative to
%   the directory of the current module's source file.
%
%   @param Spec The specification of the file, which can be a relative or
%   absolute path.
%   @param Abs The absolute file path of the specified file.
%   @param Options Additional options for the absolute file name
%   resolution.

context_file(Spec, Abs, Options) :-
    context_module(M),
    module_property(M, file(File)),
    file_directory_name(File, Directory),
    absolute_file_name(Spec, Abs, [relative_to(Directory)|Options]).
