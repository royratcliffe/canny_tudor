/*  File:    ollama/chat.pl
    Author:  Roy Ratcliffe
    Created: May 31 2025
    Purpose: Ollama Chat

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

:- module(ollama_chat,
          [ ollama_chat/3 % +Messages:list, -Message:dict, +Options:list
          ]).
:- use_module(library(http/http_open), [http_open/3]).
:- use_module(library(http/http_client), [http_read_data/3]).
:- use_module(library(settings), [setting/4, setting/2]).
:- use_module(library(option), [option/3, select_option/4]).

:- ensure_loaded(library(http/http_json)).

:- setting(ollama_url, atom, env('OLLAMA_URL', 'http://localhost:11434/api/chat'),
           'URL of Ollama API.').
:- setting(ollama_model, string, env('OLLAMA_MODEL', "nemotron-mini"),
           'Ollama model to use.').
:- setting(ollama_tools, list(dict), [],
           'Ollama tools to use.').

/** <module> Ollama Chat

Idiomatic SWI-Prolog HTTP client module for interacting with an Ollama
chat API.

## Usage

How to use and abuse the interface? Take some examples. The following
queries run with HTTP debugging enabled. Notice the headers.

For streaming:
```
?- ollama_chat([_{role:user, content:"Hello"}], Message, [stream(true)]).
% http_open: Connecting to localhost:11434 ...
%       ok <stream>(000001a4454d2630) ---> <stream>(000001a4454d2740)
% HTTP/1.1 200 OK
% Content-Type: application/x-ndjson
% Date: Sat, 31 May 2025 10:48:49 GMT
% Connection: close
% Transfer-Encoding: chunked
Message = _{content:" Hello", role:"assistant"} ;
Message = _{content:"!", role:"assistant"} ;
Message = _{content:" How", role:"assistant"} ;
Message = _{content:" can", role:"assistant"} ;
Message = _{content:" I", role:"assistant"} ;
Message = _{content:" assist", role:"assistant"} ;
Message = _{content:" you", role:"assistant"} ;
Message = _{content:" today", role:"assistant"} ;
Message = _{content:"?", role:"assistant"} ;
Message = _{content:"", role:"assistant"}.
```

The streaming content type is **not** "application/json" but rather
newline-delimited JSON. This is correct. Our addition to the JSON type
multifile predicate catches this.

For non-streaming:
```
?- ollama_chat([_{role:user, content:"Hello"}], Message, [stream(false)]).
% http_open: Connecting to localhost:11434 ...
%       ok <stream>(000001a4454d3ea0) ---> <stream>(000001a4454d4e90)
% HTTP/1.1 200 OK
% Content-Type: application/json; charset=utf-8
% Date: Sat, 31 May 2025 10:50:04 GMT
% Content-Length: 347
% Connection: close
Message = _{content:" Sure, I'm here to help! How can I assist you today?", role:"assistant"}.
```

*/

:- multifile http_json:json_type/1.

http_json:json_type(application/'x-ndjson').

%!  ollama_chat(+Messages:list(dict), -Message:dict, +Options:list) is
%!              nondet.
%
%   Leverages  SWI-Prolog's  HTTP  libraries   for    Ollama   chat  API
%   interaction. To stream or not  to   stream?  That becomes an option,
%   specify either `stream(true)`  or   `stream(false)`,  defaulting  to
%   streaming.  This  option  selects    the   predicate's  determinism.
%   Predicate   `ollama_chat/3`   becomes     non-deterministic   *when*
%   streaming, but falls back to deterministic when not.
%
%   Pulls out the message from the reply;   it becomes the result of the
%   chat interaction: many messages in, one message out. Taking only the
%   message assumes that the other keys within the reply dictionary have
%   less value. Callers can usually ignore   them. The predicate unifies
%   with reply(Reply) in the Options  argument   if  the caller wants to
%   view the detailed response information.
%
%   Assumes that the reply is  always   a  dictionary type without first
%   checking. The implementation relies on   the lower-level HTTP layers
%   for parsing and rendering the  correct   term  type. It also assumes
%   that the dictionary always contains  a   "message"  pair.  Throws an
%   exception when this presumption fails;  this   is  a  design feature
%   because all responses must have a message.

ollama_chat(Messages, Message, Options) :-
    option(stream(Stream), Options, true),
    setting(ollama_model, DefaultModel),
    option(model(Model), Options, DefaultModel),
    setting(ollama_tools, DefaultTools),
    option(tools(Tools), Options, DefaultTools),
    option(reply(Reply), Options, Reply),
    setting(ollama_url, DefaultURL),
    option(url(URL), Options, DefaultURL),
    chat(URL, _{stream:Stream,
                model:Model,
                messages:Messages,
                tools:Tools}, Reply, Options),
    Message = Reply.message.

chat(URL, Dict, Reply, Options) :-
    http_open(URL, In, [post(json(Dict)), headers(Headers)|Options]),
    call_cleanup(read_dict(In, Reply, Headers), close(In)).

read_dict(In, Dict, Options) :-
    read_data(In, Dict, [json_object(dict)|Options]),
    (   get_dict(done, Dict, false)
    ->  true
    ;   !
    ).

read_data(In, NotEndOfFileData, Options) :-
    select_option(end_of_file(EndOfFile), Options, Options_, end_of_file),
    repeat,
    (   http_read_data([input(In)], Data, [end_of_file(EndOfFile)|Options_]),
        Data \== EndOfFile
    ->  NotEndOfFileData = Data
    ;   !,
        fail
    ).
