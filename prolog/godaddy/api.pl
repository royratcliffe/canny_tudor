/*  File:    godaddy_api.pl
    Author:  Roy Ratcliffe
    Created: Oct 30 2021
    Purpose: GoDaddy API

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

:- module(godaddy_api,
          [ godaddy_domain/1,                   % -Domain:dict
            godaddy_domain_record/2,            % -Domain,-Record
            godaddy_domain_record_data/4,       % +Domain,+Type,+Name,+Data
            get_godaddy/3,                      % +Path,-Data,+Options
            post_godaddy/4                      % +Path,+Data,-Reply,+Options
          ]).
:- use_module(library(settings), [setting/4, setting/2]).
:- use_module(library(url), [parse_url/2]).
:- use_module(library(http/http_client), [http_get/3]).

:- ensure_loaded(library(http/http_json)).

/** <module> godaddy_api

You can set up the API key and secret from within Prolog.
```prolog
setenv('GODADDY_API_KEY', 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX').
setenv('GODADDY_API_SECRET', 'XXXXXXXXXXXXXXXXXXXXXX').
```
Format all active records using:
```prolog
forall(godaddy_domain_record(A, B),
       format('~s ~s.~s ~s~n', [B.type, B.name, A.domain, B.data])).
```
@see https://developer.godaddy.com/doc

*/

:- setting(host, atom, 'api.godaddy.com', 'GoDaddy API host').
:- setting(key, atom, env('GODADDY_API_KEY'), 'GoDaddy API key').
:- setting(secret, atom, env('GODADDY_API_SECRET'), 'GoDaddy API secret').

%!  godaddy_domain(-Domain:dict) is nondet.
%
%   Zero or more GoDaddy Domain dictionaries.
%
%   The following unifies with all currently-active domains.
%
%       godaddy_domain(Domain), Domain.status == "ACTIVE".

godaddy_domain(Domain) :-
    get_godaddy('/v1/domains', Domains, [json_object(dict)]),
    member(Domain, Domains).

%!  godaddy_domain_record(-Domain:dict, -Record:dict) is nondet.
%
%   Unifies with all domains records at Record and Domain. Automatically
%   filters for active domains. Attempting to access records for
%   non-active domains throws an `existence_error` exception.

godaddy_domain_record(Domain, Record) :-
    godaddy_domain(Domain),
    Domain.status == "ACTIVE",
    format(atom(Path), '/v1/domains/~s/records', [Domain.domain]),
    get_godaddy(Path, Records, [json_object(dict)]),
    member(Record, Records).

%!  godaddy_domain_record_data(+Domain:atom, +Type:atom,
%!  +Name:atom, +Data) is det.
%
%   Updates domain record data. Creates a record if none currently
%   exists. Data must be a single JSON term or dictionary.
%
%   The following updates the A-record for `www` at some domain with
%   an hour for time to live.
%
%   ```
%   godaddy_domain_record_data("swi-prolog.org", "A", www,
%                              _{ data:"1.1.1.1",
%                                 ttl:3600
%                               }).
%   ```
%
%   @error existence_error if Type is not a valid record type.

godaddy_domain_record_data(Domain, Type, Name, Data) :-
    format(atom(Path), '/v1/domains/~s/records/~s/~s', [Domain, Type, Name]),
    post_godaddy(Path, [Data], '', [method(put)]).

%!  get_godaddy(+Path, -Data, +Options) is det.
%
%   Gets data from the GoDaddy API using an authorised and secure
%   HTTP GET request with JSON encoding. Switch from term-based JSON to
%   dictionary-based using option `json_object(dict)` to override the
%   default.

get_godaddy(Path, Data, Options) :-
    url(Path, URL),
    options(Options, Options1),
    http_get(URL, Data, Options1).

%!  post_godaddy(Path, Data, Reply, Options) is det.
%
%   Posts, or puts or patches if you override using the `method` option,
%   to the GoDaddy API.
%
%   Automatically encodes the outgoing Data as JSON; no need to encode
%   the payload within a `json/1` functor. You can pass either a JSON
%   term or a dictionary, typically enclosed within a list.

post_godaddy(Path, Data, Reply, Options) :-
    url(Path, URL),
    options(Options, Options1),
    http_post(URL, json(Data), Reply, Options1).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    H E L P E R S

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

url(Path, URL) :-
    setting(host, Host),
    parse_url(URL, [protocol(https), host(Host), path(Path)]).

options(Options, [ request_header('Authorization'=Authorization)
                 | Options
                 ]) :-
    setting(key, Key),
    setting(secret, Secret),
    format(atom(Authorization), 'sso-key ~s:~s', [Key, Secret]).
