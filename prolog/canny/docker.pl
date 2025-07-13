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
          [ docker/3                            % +Operation, -Reply, +Options
          ]).
:- autoload(library(apply), [convlist/3]).
:- autoload(library(atom), [restyle_identifier/3]).
:- autoload(library(lists), [append/3, reverse/2, member/2]).
:- autoload(library(option), [option/2]).
:- autoload(library(dcg/basics), [string_without/4]).
:- autoload(library(http/http_client), [http_get/3]).
:- autoload(library(http/json), [json_read_dict/2]).
:- use_module(library(settings), [setting/4, setting/2]).

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

### Example usage

The `url_options/3` predicate can be used to construct the URL and options for
a specific Docker operation. For example, to get the URL and options for the
`system_ping` operation, you can use:

```prolog
?- [library(http/http_client)].
true.

?- canny_docker:url_options(system_ping, URL, Options), http_get(URL, Reply, Options).
URL = [path('/v1.49/_ping'), protocol(tcp), host(localhost), port(2375)],
Options = [method(get), accept(["text/plain"])],
Reply = 'OK'.
```

For listing containers, you can use:

```prolog
?- canny_docker:url_options(container_list, URL, Options), http_get(URL, Reply, Options).
URL = [path('/v1.49/containers/json'), protocol(tcp), host(localhost), port(2375)],
Options = [method(get), accept(["application/json"])],
Reply = [json(['Id'='12a42bbfcc5f64967da12ac03d46e0a3b885b104f1e1e2a0ecd27cea31fb1579', ...|...])].
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
%   `path_and_method/3`, and the resulting options are suitable for making
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
    url_options(Version, Operation, Path_, Options_),
    format_path(Path_, Path, Options),
    select_option(search(Search), Options, Options0, []),
    append(Options0, Options_, Options__),
    http_get([path(Path), search(Search)|URL], Reply, Options__).

%!  format_path(+Format:atom, -Path:atom, +Options:list) is det.
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
    atom_codes(Format, Codes),
    phrase(format_path([], Atomics_, Options), Codes),
    reverse(Atomics_, Atomics),
    atomic_list_concat(Atomics, '', Path).

format_path(Atomics0, Atomics, Options) -->
    "{",
    string_without("}", NameCodes),
    "}",
    !,
    { atom_codes(Name, NameCodes),
      Option =.. [Name, Value],
      option(Option, Options)
    },
    format_path([Value|Atomics0], Atomics, Options).
format_path(Atomics0, Atomics, Options) -->
    [Code],
    !,
    { atom_codes(Atom, [Code])
    },
    format_path([Atom|Atomics0], Atomics, Options).
format_path(Atomics, Atomics, _Options) -->
    [].

%!  url_options(+Version, ?Operation, -Path, -Options) is semidet.
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
%   @param Options List of options for the URL, such as `method` and `accept`.

url_options(Version, Operation, Path, [method(Method)|Options]) :-
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

:- table operation/5.

operation(Version, Operation, Path, Method, Options) :-
    load_docker_api_json(Version, Dict),
    path_and_method(Dict.paths, Path, Method, MethodDict),
    get_dict(operationId, MethodDict, OperationId),
    restyle_identifier(one_two, OperationId, Operation),
    dict_pairs(MethodDict, _, Pairs),
    convlist(method_option, Pairs, Options).

method_option(produces-Produces, accept(Produces)).

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
%   @param MethodDict Dictionary with details for the specified method.

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
