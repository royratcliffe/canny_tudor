/*  File:    redis/tilde.pl
    Author:  Roy Ratcliffe
    Created: Jun 26 2026
    Purpose: Redis tilde operations

Copyright (c) 2026, Roy Ratcliffe, Northumberland, United Kingdom

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

:- module(redis_tilde,
          [ op(440, xfx, ~>),
            op(550, fx, ~),
            (~>)/2,             % +Request, -Reply
            (~)/1               % +PipeLine
          ]).
:- autoload(library(redis), [redis/3, redis/2]).
:- use_module(library(settings), [setting/4, setting/2]).

:- setting(server, atom, default, 'Server to use for Redis tilde operations').

/** <module> Redis Tilde Operations
 *
 * This module provides operators for interacting   with  a Redis server
 * using the tilde syntax. The `~>` operator   is used to send a request
 * to the Redis server and retrieve a   reply, while the `~` operator is
 * used to send a pipeline of requests.
 *
 * The Redis server to use for these  operations can be configured using
 * the `server` setting. By default, it is  set to `default`, but it can
 * be changed to any valid Redis server identifier.
 *
 * ## Example Usage
 *
 * Read the current time from the Redis server and convert it to seconds:
 * ```prolog
 * ?- [library(redis/tilde)].
 * ?- time ~> [S, US], Time is S + (US / 1e6).
 * ```
 *
 * @author Roy Ratcliffe
 * @version 1.0
 * @license MIT
 */

:- op(440, xfx, ~>).

%!  ~>(+Request, -Reply) is semidet.
%
%   Sends a request to the Redis server   and  retrieves the reply. Uses
%   the Redis server specified in the settings.
%
%   @arg Request The request to send to the Redis server.
%
%   @arg Reply The reply received from the Redis server.

~>(Request, Reply) :-
    setting(server, Redis),
    redis(Redis, Request, Reply).

:- op(550, fx, ~).

%!  ~(+PipeLine) is det.
%
%   Sends a pipeline of requests to the Redis server.
%
%   @arg PipeLine The pipeline of requests to send to the Redis server.

~(PipeLine) :-
    setting(server, Redis),
    redis(Redis, PipeLine).
