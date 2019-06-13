:- module(os_apps,
          [   start_app/1,
              stop_app/1
          ]).

:- meta_predicate
    detach(0, +).

/*
 * There is an important distinction between programs and processes.
 * These predicates use processes to launch programs. Programs in this
 * sense is more akin to an application, where one application has
 * typically one instance; else if not has differing arguments to
 * distinguish one from another. Hence for the same reason, the model
 * ignores standard input. Programs as apps have no standard input,
 * conceptually speaking.
 *
 * For the same reason, the predicates rely on multi-file
 * os:app_property/2 to configure the program launch
 * path, arguments and options.
 *
 */

:- dynamic
    app_pid/2.

:- multifile
    os:app_property/2.

os:app_property(App, running) :-
    app_pid(App, _).
os:app_property(App, pid(PID)) :-
    app_pid(App, PID).

%!  start_app(?App:compound) is nondet.
%
%   Starts an App if not already running.
%
%   An app's argument and option properties execute
%   non-deterministically.
%
%   Options can include the following:
%
%       * encoding(Encoding)
%       an encoding option for the output and error streams.
%
%       * alias(Alias)
%       an alias prefix for the detached watcher threads.
%
%   Checks for not-running *after* unifying with the App path.

start_app(App) :-
    os:app_property(App, path(Path)),
    \+ os:app_property(App, running),
    findall(Arg, os:app_property(App, argument(Arg)), Args),
    findall(Opt, os:app_property(App, option(Opt)),  Opts),
    include(current_predicate_option(process_create/3, 3), Opts, Opts_),
    process_create(Path, Args,
                   [   stdout(pipe(Out)),
                       stderr(pipe(Err)),
                       process(PID)|Opts_
                   ]),
    assertz(app_pid(App, PID)),
    option(alias(Alias), Opts, PID),
    (   option(encoding(Encoding), Opts)
    ->  set_stream(Out, encoding(Encoding)),
        set_stream(Err, encoding(Encoding))
    ;   true
    ),
    detach(wait_for_process(PID), [Alias, pid]),
    detach(read_lines_to_codes(App, stdout(Out)), [Alias, out]),
    detach(read_lines_to_codes(App, stderr(Err)), [Alias, err]).

detach(Callable, Aliases) :-
    atomic_list_concat(Aliases, '_', Alias),
    thread_create(Callable, _, [detached(true), alias(Alias)]).

wait_for_process(PID) :-
    app_pid(App, PID),
    broadcast(os:app_started(App)),
    process_wait(PID, Status),
    retract(app_pid(App, PID)),
    broadcast(os:app_stopped(App, Status)).

read_lines_to_codes(App, Term0) :-
    Term0 =.. [Name, Stream],
    repeat,
        read_line_to_codes(Stream, Codes),
        (   Codes == end_of_file
        ->  true
        ;   Term =.. [Name, Codes],
            catch(
                broadcast(os:app_decoded(App, Term)),
                Catcher,
                print_message(error, Catcher)),
            fail
        ),
        close(Stream).

%!  stop_app(?App:compound) is nondet.

stop_app(App) :-
    os:app_property(App, pid(PID)),
    process_kill(PID).

:- multifile
    user:go/1,
    user:nogo/1.

:- public
    user:go/1.

user:go(debug(app)) :-
    user:nogo(debug(app)),
    debug(app),
    listen(os:app_started(App), started(App)),
    listen(os:app_stopped(App, Status), stopped(App, Status)),
    listen(os:app_decoded(App, Codes), decoded(App, Codes)).

user:nogo(debug(app)) :-
    context_module(Module),
    unlisten(Module),
    nodebug(app).

started(App) :-
    debug(app, 'started ~p', [App]).

stopped(App, Status) :-
    debug(app, 'stopped ~p ~p', [App, Status]).

decoded(App, Codes) :-
    Codes =.. [Name, Codes0],
    debug(app, 'decoded ~p ~s ~s', [App, Name, Codes0]).
