:- module(os_apps,
          [   start_app/1,              % ?App:compound
              stop_app/1                % ?App:compound
          ]).

:- meta_predicate
    detach(0, +).

/** <module> Operation system apps
 *
 * What is an app? In an operating-system =os_apps= module context,
 * simply something you can start and stop using a process. It has no
 * standard input, and typically none or minimal standard output and
 * error. Apps start by creating a process. Processes have four distinct
 * inputs: a path specification, a list of arguments, possibly some
 * execution options along with some optional encoding and other
 * run-time related options. Call this the application's configuration.
 * This may seem like an unusual definition of "app" and maybe too
 * technical, but English limits the alternatives: process, no because
 * that means something that loads an app; program, no because that
 * generally refers the app's image including its resources.
 *
 * There is an important distinction between apps and processes. These
 * predicates use processes to launch apps. An application typically has
 * one instance; else if not has differing arguments to distinguish one
 * running instance of the app from another. Hence for the same reason,
 * the app model here ignores standard input. Apps have no standard
 * input, conceptually speaking.
 *
 * For the same reason, the predicates rely on multi-file
 * os:app_property/2 to configure the app launch path, arguments and
 * options.
 *
 * For all the above reasons, the special multi-file predicate
 * os:app_property/2 supplies an app's configuration deterministically
 * using four sub-terms for the second argument, as follows.
 *
 *   - os:app_property(App, path(Path))
 *   - os:app_property(App, argument(Argument))
 *   - os:app_property(App, option(Option))
 *
 * Two things to note about these predicate calls; (1) App is a compound
 * describing the app and its app-specific configuration information;
 * (2) the second Property argument collates arguments and options
 * non-deterministically. Predicate start_app/1 finds all the argument-
 * and option-solutions in the order defined.
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
%   Starts an App if not already running. Starts more than one apps
%   non-deterministically if App binds with more than one specifier.
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
