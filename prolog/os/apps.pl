:- module(os_apps,
          [   app_property/2,           % ?App:compound, ?Property
              app_start/1,              % ?App:compound
              app_stop/1,               % ?App:compound
              app_up/1,                 % ?App:compound
              app_down/1                % ?App:compound
          ]).

:- meta_predicate
    detach(0, +).

/** <module> Operation system apps
 *
 * What is an app? In this   operating-system  =os_apps= module context,
 * simply something you can start and stop   using  a process. It has no
 * standard input, and typically none  or   minimal  standard output and
 * error.
 *
 * There is an important distinction between   apps and processes. These
 * predicates use processes to launch apps. An application typically has
 * one process instance;  else  if  not,   has  differing  arguments  to
 * distinguish one running instance of the   app from another. Hence for
 * the same reason, the app model   here  ignores "standard input." Apps
 * have no such input stream, conceptually speaking.
 *
 * Is "app" the right word to describe  such a thing? English limits the
 * alternatives: process, no because that means  something that loads an
 * app; program, no because  that  generally   refers  the  app's  image
 * including its resources.
 *
 * ## App configuration
 *
 * Apps start by creating  a  process.   Processes  have  four  distinct
 * specification parameter groups:  a  path   specification,  a  list of
 * arguments, possibly some execution options   along with some optional
 * encoding  and  other  run-time  related    options.   Call  this  the
 * application's configuration.
 *
 * The =os_apps= predicates rely on  multi-file os:property_for_app/2 to
 * configure  the  app  launch   path,    arguments   and  options.  The
 * property-for-app   predicate   supplies   an    app's   configuration
 * non-deterministically using three sub-terms for   the  first Property
 * argument, as follows.
 *
 *   - os:property_for_app(path(Path), App)
 *   - os:property_for_app(argument(Argument), App)
 *   - os:property_for_app(option(Option), App)
 *
 * Two things to note about these  predicates;   (1)  App  is a compound
 * describing the app *and* its  app-specific configuration information;
 * (2) the first  Property  argument   collates  arguments  and  options
 * non-deterministically. Predicate app_start/1 finds  all the argument-
 * and option-solutions _in the order defined_.
 *
 * ## Start up and shut down
 *
 * By default, starting an app does *not*   persist the app. It does not
 * restart if the user or some other   agent, including bugs, causes the
 * app  to  exit.  Consequently,   this    module   offers  a  secondary
 * app-servicing layer. You can start up  or   shut  down  any app. This
 * amounts  to  starting  and  upping  or   stopping  and  downing,  but
 * substitutes shut for stop.  Starting  up   issues  a  start  but also
 * watches for stopping.
 *
 * ## Broadcasts
 *
 * Sends three broadcast messages for any given App, as follows:
 *
 *   - os:app_started(App)
 *   - os:app_decoded(App, stdout(Codes))
 *   - os:app_decoded(App, stderr(Codes))
 *   - os:app_stopped(App, Status)
 *
 * Running apps send zero or   more  os:app_decoded(App, Term) messages,
 * one for every line appearing in   their  standard output and standard
 * error streams. Removes line terminators.   App termination broadcasts
 * an exit(Code) term for its final Status.
 *
 */

:- dynamic app_pid/2.

%!  app_property(?App:compound, ?Property) is nondet.
%
%   Property of App.
%
%   Note  that  app_property(App,  defined)  should    *not*   throw  an
%   exception. Some apps have  an   indeterminate  number of invocations
%   where App is a compound with variables. Make sure that the necessary
%   properties are ground, rather than unbound.
%
%   Collapses non-determinism to  determinism  by   collecting  App  and
%   Property   pairs   before   expanding    the     bag    to   members
%   non-deterministically.

app_property(App, Property) :-
    bagof(App-Property, os:property_for_app(Property, App), Bag),
    member(App-Property, Bag).

:- multifile os:property_for_app/2.

os:property_for_app(defined, App) :-
    os:property_for_app(path(_), App).
os:property_for_app(running, App) :-
    app_pid(App, _).
os:property_for_app(pid(PID), App) :-
    app_pid(App, PID).

%!  app_start(?App:compound) is nondet.
%
%   Starts an App if not already  running.   Starts  more  than one apps
%   non-deterministically if App binds  with   more  than one specifier.
%   Does not restart the  app  if   launching  fails.  See  app_up/1 for
%   automatic restarts. An app's argument  and option properties execute
%   non-deterministically.
%
%   Options can include the following:
%
%       * encoding(Encoding)
%       an encoding option for the output and error streams.
%
%       * alias(Alias)
%       an alias prefix for the detached watcher thread.
%
%   Checks for not-running *after* unifying with  the App path. Succeeds
%   if already running.

app_start(App) :-
    app_property(App, defined),
    app_start_(App).

app_start_(App) :-
    app_property(App, running),
    !.
app_start_(App) :-
    app_property(App, path(Path)),
    findall(Arg, app_property(App, argument(Arg)), Args),
    findall(Opt, app_property(App, option(Opt)),  Opts),
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

%!  detach(:Goal, +Aliases:list(atom)) is det.
%
%   Important to assert  the  app_pid(App,   PID)  before  detaching the
%   threads. They will unify with the App in order to access the process
%   identifier, PID. Note assertz/1 usage above.

detach(Goal, Aliases) :-
    atomic_list_concat(Aliases, '_', Alias),
    thread_create(Goal, _, [detached(true), alias(Alias)]).

%!  wait_for_process(+App) is semidet.
%
%   Waits for App to exit  in  its   own  detached  thread. Retracts the
%   App-PID pair immediately  after  process   exit.  Broadcasts  an App
%   stopped message with the  process  exit   status.  This  is the sole
%   purpose of the wait.

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

%!  app_stop(?App:compound) is nondet.
%
%   Kills the App process. Stopping the  app does not prevent subsequent
%   automatic restart.
%
%   Killing does *not* retract the app_pid/2   by design. Doing so would
%   trigger a failure warning. (The waiting PID-monitor thread would die
%   on failure because its retract attempt fails.)

app_stop(App) :-
    app_property(App, pid(PID)),
    process_kill(PID).

:- dynamic app/1.

%!  app_up(?App:compound) is nondet.
%
%   Starts up an App.
%
%   Semantics of this predicate rely on   app_start/1 succeeding even if
%   already started. That way, you can   start  an app then subsequently
%   _up_ it, meaning stay up. Hence, you   can  app_stop(App) to force a
%   restart if already app_up(App). Stopping an app does not _down_ it!
%
%   Note that app_start/1 will fail for one  of two reasons: (1) because
%   the App has not been defined yet;  (2) because starting it fails for
%   some reason.

app_up(App) :-
    app_property(App, defined),
    app_up_(App).

app_up_(App) :-
    app(App),
    !.
app_up_(App) :-
    app_start(App),
    assertz(app(App)).

os:property_for_app(up, App) :-
    app(App).
os:property_for_app(down, App) :-
    \+ app(App).

%!  app_down(?App:compound) is nondet.
%
%   Shuts down an App. Shuts down multiple apps non-deterministically if
%   the App compound matches more than one app definition.

app_down(App) :-
    retract(app(App)),
    app_stop(App).

listen :-
    unlisten,
    listen(os:app_stopped(App, _), app_stopped(App)).

unlisten :-
    context_module(Module),
    unlisten(Module).

:- initialization listen.

%!  app_stopped(+App) is semidet.
%
%   The broadcast triggers in the PID-monitoring   thread. Do not try to
%   restart the app in the same thread.   Starting tries to create a new
%   PID-monitoring thread with the same alias, if   an alias for the app
%   has been given. This will fail since  the current thread carries the
%   same alias from the  previous  start   operation.  Avoid  this  race
%   condition by restarting the app _after_ the broadcast thread exits.

app_stopped(App) :-
    app(App),
    thread_create(app_start(App), _, [detached(true)]).
