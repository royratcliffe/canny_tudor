:- module(gh_api,
          [ ghapi_update_gist/4,                 % +GistID,+Data,-Reply,+Options
            ghapi_get/3                          % +PathComponents,+Data,+Options
          ]).
:- autoload(library(option), [option/2]).
:- autoload(library(url), [parse_url/2]).
:- autoload(library(http/http_client), [http_get/3]).
:- use_module(library(settings), [setting/4, setting/2]).

/** <module> GitHub API
 *
 * You need a personal access token for updates. You do *not* require
 * them for public access.
 *
 * @author Roy Ratcliffe
 */

:- setting(access_token, atom, env(ghapi_access_token, ''),
           'GitHub API personal access token').

%!  ghapi_update_gist(+GistID, +Data, -Reply, +Options) is det.
%
%   Updates a Gist by its unique identifier. Data is the patch payload
%   as a JSON object, or dictionary if you include json_object(dict) in
%   Options. Reply is the updated Gist in JSON on success.
%
%       ghapi_update_gist(
%           ec92ac84832950815861d35c2f661953,
%           json(json([ files=json([ 'cov.json'=json([ content='{}'
%                                                    ])
%                                  ])
%                     ])), _, []).
%
%   @see https://docs.github.com/en/rest/reference/gists#update-a-gist

ghapi_update_gist(GistID, Data, Reply, Options) :-
    ghapi_get([gists, GistID], Reply, [method(patch), post(Data)|Options]).

%!  ghapi_get(+PathComponents, +Data, +Options) is det.
%
%   Accesses the GitHub API. Supports JSON terms and dictionaries. For
%   example, the following goal accesses the GitHub Gist API looking for
%   a particular Gist by its identifier and unifies A with a JSON term
%   representing the Gist's current contents and state.
%
%       ghapi_get([gists, ec92ac84832950815861d35c2f661953], A, []).
%
%   Supports all HTTP methods despite the predicate name. The "get"
%   mirrors the underlying http_get/3 method which also supports all
%   methods. POST and PATCH send data using the post/1 option and
%   override the default HTTP verb using the method/1 option. Similarly
%   here.
%
%   Handles authentication via settings, and from the system environment
%   indirectly. Option ghapi_access_token/1 overrides both. Order of
%   overriding proceeds as: option, setting, environment, none. Empty
%   atom counts as none.
%
%   Abstracts away the path using path components. Argument
%   PathComponents is an atomic list specifying the URL path.

ghapi_get(PathComponents, Data, Options) :-
    ghapi_get_options(Options_, Options),
    atomic_list_concat([''|PathComponents], /, Path),
    parse_url(URL, [protocol(https), host('api.github.com'), path(Path)]),
    http_get(URL, Data,
             [ request_header('Accept'='application/vnd.github.v3+json')
             | Options_
             ]).

ghapi_get_options([ request_header('Authorization'=Authorization)
                  | Options
                  ], Options) :-
    ghapi_access_token(AccessToken, Options),
    AccessToken \== '',
    !,
    format(atom(Authorization), 'token ~s', [AccessToken]).
ghapi_get_options(Options, Options).

ghapi_access_token(AccessToken, Options) :-
    option(ghapi_access_token(AccessToken), Options),
    !.
ghapi_access_token(AccessToken, _Options) :-
    setting(access_token, AccessToken).
