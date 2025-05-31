/*  File:    ollama/chat.pl
    Author:  Roy Ratcliffe
    Created: May 31 2025
    Purpose: Ollama Chat
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

/** <module> Ollama Chat

*/

:- multifile http_json:json_type/1.

http_json:json_type(application/'x-ndjson').

%!  ollama_chat(+Messages:list(dict), -Message:dict, +Options:list) is
%!              nondet.

ollama_chat(Messages, Message, Options) :-
    option(stream(Stream), Options, true),
    setting(ollama_model, DefaultModel),
    option(model(Model), Options, DefaultModel),
    option(reply(Reply), Options, Reply),
    setting(ollama_url, DefaultURL),
    option(url(URL), Options, DefaultURL),
    chat(URL, _{stream:Stream,
                model:Model,
                messages:Messages}, Reply, Options),
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
