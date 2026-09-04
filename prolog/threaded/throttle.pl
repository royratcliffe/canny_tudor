/*  File:    threaded/throttle.pl
    Author:  Roy Ratcliffe
    Created: Jul  7 2026
    Purpose: Threaded throttle mechanism for Redis streams

Copyright (c) 2026, Roy Ratcliffe, Northumberland, United Kingdom

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

:- module(threaded_throttle,
          [ consume_threaded_throttle/2 % +Key, +Field
          , unconsume_threaded_throttle/2 % +Key, +Field
          , current_threaded_throttle_timeout/4 % +When, +Key, +Field, -Timeout
          , set_threaded_throttle_timeout/4 % +When, +Key, +Field, +Timeout
          , exit_threaded_throttle/2 % +Key, +Field
          ]).
:- autoload(library(broadcast), [listen/3, unlisten/1, broadcast/1]).
:- autoload(library(redis), [redis/3]).
:- use_module(library(settings), [setting/4, setting/2]).

/** <module> Threaded Throttle
 *
 * This module provides functionality for consuming throttle events from
 * a Redis stream in a threaded manner. It allows for the creation of
 * threads to handle throttle events, with configurable idle and wait
 * timeouts. The module also provides predicates to set and retrieve
 * timeout values for specific keys and fields.
 *
 * ---+++ Broadcast Events
 *
 * The module broadcasts the following events:
 *
 * - threaded_throttle(up, Key, Field) : A throttle event has been
 * received for the specified Key and Field.
 *
 * - threaded_throttle(adjust, Key, Field, Value) : The throttle value
 * has been adjusted for the specified Key and Field.
 *
 * - threaded_throttle(down, Key, Field) : The throttle event has
 * completed for the specified Key and Field.
 *
 */

:- setting(idle_timeout, number, env('THREADED_THROTTLE_IDLE_TIMEOUT', 5),
           'Timeout in seconds for idling').

:- setting(wait_timeout, number, env('THREADED_THROTTLE_WAIT_TIMEOUT', 5),
           'Timeout in seconds for waiting').

%!  consume_threaded_throttle(+Key:atom, +Field:atom) is det.
%
%   Listens to a Redis stream for throttle events on the specified Key
%   and Field. When an event is received, it creates a thread to handle
%   the event and sends the value to that thread.
%
%   @arg Key The Redis stream key to listen to.
%
%   @arg Field The field within the Redis stream to listen for events.

consume_threaded_throttle(Key, Field) :-
    % Use the thread alias as the listener to avoid duplicate listeners for the
    % same Key and Field.
    key_field_alias(Key, Field, Alias),
    listen(Alias, redis_consume(Key, Entry, _), consume(Key, Field, Entry)).

%!  unconsume_threaded_throttle(+Key:atom, +Field:atom) is det.
%
%   Stops listening to the Redis stream for throttle events on the
%   specified Key and Field.
%
%   @arg Key The Redis stream key to stop listening to.
%
%   @arg Field The field within the Redis stream to stop listening for
%   events.

unconsume_threaded_throttle(Key, Field) :-
    key_field_alias(Key, Field, Alias),
    unlisten(Alias).

consume(Key, Field, Entry) :-
    get_dict(Field, Entry, Value),
    !,
    % Lazily create a thread for the Key and Field if it doesn't exist, and
    % send the Value to that thread.
    create_thread(Key, Field, Thread),
    thread_send_message(Thread, Value).
consume(_, _, _).

% TODO: make the alias construction injective so that it is unique for
% each Key and Field combination. This will prevent potential conflicts
% if different Key and Field combinations produce the same alias.
key_field_alias(Key, Field, Alias) :-
    atomic_list_concat([threaded, Key, Field, throttle], '_', Alias).

create_thread(Key, Field, Thread) :-
    key_field_alias(Key, Field, Alias),
    (   thread_property(Thread, alias(Alias))
    ->  true
    ;   thread_create(catch(throttle(Key, Field), quit, true), Thread,
                      [ alias(Alias),
                        detached(true)
                      ])
    ).

throttle(Key, Field) :-
    (   redis(default, get(Key:Field), Value)
    ->  throttle_(Key, Field, Value)
    ;   throttle____(Key, Field)
    ).

throttle_(Key, Field, Value) :-
    broadcast(threaded_throttle(up, Key, Field)),
    throttle__(Key, Field, Value).

throttle__(Key, Field, Value) :-
    broadcast(threaded_throttle(adjust, Key, Field, Value)),
    redis(default, set(Key:Field, Value), status(ok)),
    throttle___(Key, Field, Value).

throttle___(Key, Field, Value) :-
    thread_self(Self),
    current_threaded_throttle_timeout(idle, Key, Field, Timeout),
    (   thread_get_message(Self, Value1, [timeout(Timeout)])
    ->  (   thread_peek_message(Self, _)
        ->  throttle___(Key, Field, Value)
        ;   (   Value == Value1
            ->  throttle___(Key, Field, Value1)
            ;   throttle__(Key, Field, Value1)
            )
        )
    ;   throttle____(Key, Field)
    ).

throttle____(Key, Field) :-
    broadcast(threaded_throttle(down, Key, Field)),
    thread_self(Self),
    current_threaded_throttle_timeout(wait, Key, Field, Timeout),
    (   thread_get_message(Self, Value, [timeout(Timeout)])
    ->  throttle_(Key, Field, Value)
    ;   throttle(Key, Field)
    ).

:- dynamic timeout/4.

%!  current_threaded_throttle_timeout(+When:atom,
%!                                    +Key:atom,
%!                                    +Field:atom,
%!                                    -Timeout:number) is det.
%
%   Retrieves the current timeout value for the specified Key and Field
%   based on the When condition (idle or wait). If a specific timeout
%   has been set for the Key and Field, it will be used; otherwise, the
%   default setting will be retrieved from the application settings.
%
%   @arg When The condition for which the timeout is being retrieved
%   (idle or wait).
%
%   @arg Key The Redis stream key for which the timeout is being
%   retrieved.
%
%   @arg Field The field within the Redis stream for which the timeout
%   is being retrieved.
%
%   @arg Timeout The timeout value in seconds.

current_threaded_throttle_timeout(When, Key, Field, Timeout) :-
    when(When, TimeoutSetting),
    (   timeout(When, Key, Field, Timeout)
    ->  true
    ;   setting(TimeoutSetting, Timeout)
    ).

%!  set_threaded_throttle_timeout(+When:atom,
%!                                +Key:atom,
%!                                +Field:atom,
%!                                +Timeout:number) is det.
%
%   Sets a specific timeout value for the specified Key and Field based
%   on the When condition (idle or wait). This allows for dynamic
%   adjustment of timeout values for different throttle events.
%
%   @arg When The condition for which the timeout is being set (idle or
%   wait).
%
%   @arg Key The Redis stream key for which the timeout is being set.
%
%   @arg Field The field within the Redis stream for which the timeout
%   is being set.
%
%   @arg Timeout The timeout value in seconds.

set_threaded_throttle_timeout(When, Key, Field, Timeout) :-
    when(When, _),
    retractall(timeout(When, Key, Field, _)),
    assertz(timeout(When, Key, Field, Timeout)).

when(idle, idle_timeout).
when(wait, wait_timeout).

%!  exit_threaded_throttle(+Key:atom, +Field:atom) is det.
%
%   Exits the threaded throttle for the specified Key and Field by
%   signaling the associated thread to terminate. If a thread exists for
%   the Key and Field, it will be signaled to throw an exit_thread
%   exception, allowing for graceful termination of the thread.

exit_threaded_throttle(Key, Field) :-
    key_field_alias(Key, Field, Alias),
    (   thread_property(Thread, alias(Alias))
    ->  thread_signal(Thread, throw(quit))
    ;   true
    ).
