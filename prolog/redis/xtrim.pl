/*  File:    redis/xtrim.pl
    Author:  Roy Ratcliffe
    Created: Jul  7 2026
    Purpose: Redis XTRIM command wrapper for Prolog

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

:- module(redis_xtrim,
          [ xtrim_using_entry_id/4 % +Redis, +Key, +Id, +Options
          , xtrim_window_minid/3   % +Id, -MinId, +Options
          ]).
:- autoload(library(option), [option/3]).
:- autoload(library(redis), [redis/2]).
:- use_module(library(settings), [setting/4, setting/2]).

:- setting(window, number, env('REDIS_XTRIM_DEFAULT_WINDOW', 5),
    'Default window for trimming Redis stream entries (in seconds)').

/** <module> Redis XTRIM command wrapper
 *
 * This module provides predicates for interacting with Redis streams
 * using the XTRIM command. The `xtrim_using_entry_id/4` predicate trims
 * a Redis stream based on a specified entry ID and a window of time,
 * while the `xtrim_window_minid/3` predicate calculates the minimum ID
 * for trimming based on the provided entry ID and window size.
 *
 * The default window size for trimming can be configured using the
 * `window` setting. By default, it is set to 5 seconds, but it can be
 * changed to any valid number of seconds.
 *
 * ## Example Usage
 *
 * Trim a Redis stream using a specific entry ID and window size:
 * ```prolog
 * ?- xtrim_using_entry_id(Redis, Key, Id, [window(10)]).
 * ```
 *
 * @author Roy Ratcliffe
 * @version 1.0
 * @license MIT
 */

%!  xtrim_using_entry_id(+Redis, +Key, +Id, +Options) is det.
%
%   Trim the Redis stream identified by Key to remove entries older than
%   the specified Id as the upper bound and a window of time defined by
%   the window/1 option. The Id is expected to be in the format
%   "Millis-Sequence", where Millis is the timestamp in milliseconds.
%   The window size can be specified in Options, and if not provided,
%   the default window size from the window setting will be used.
%
%   @arg Redis The Redis connection.
%   @arg Key The Redis stream key to trim.
%   @arg Id The ID of the entry to use as the minimum for trimming.
%   @arg Options A list of options for trimming, including the window size.

xtrim_using_entry_id(Redis, Key, Id, Options) :-
    xtrim_window_minid(Id, MinId, Options),
    redis(Redis, xtrim(Key, minid, ~, MinId)).

%!  xtrim_window_minid(+Id, -MinId, +Options) is det.
%
%   Calculate the minimum ID for trimming based on the provided entry ID
%   and the window/1 option. The MinId is calculated by subtracting
%   the window size (in seconds) from the timestamp part of the
%   provided Id. The resulting MinId is formatted as "Millis-0" to be
%   used for trimming the Redis stream.
%
%   @arg Id The ID of the entry to use as the minimum for trimming.
%   @arg MinId The calculated minimum ID for trimming, formatted as "Millis-0".
%   @arg Options A list of options for trimming, including the window size.

xtrim_window_minid(Id, MinId, Options) :-
    % Ignore the sequence number in the entry ID and only use the timestamp for
    % trimming. The entry ID atom is expected to be in the format
    % "Millis-Sequence", where Millis is the timestamp in milliseconds.
    atomic_list_concat([Stamp0, _], -, Id),
    atom_number(Stamp0, Stamp1),
    setting(window, DefaultWindow),
    option(window(Window), Options, DefaultWindow),
    % Assume that the Stamp is always a big number: the epoch in
    % milliseconds. The window is in seconds, so multiply by 1000 to
    % convert to milliseconds.
    Stamp is ceiling(Stamp1 - (Window * 1000)),
    format(atom(MinId), '~w-0', [Stamp]).
