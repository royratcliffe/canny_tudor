/*  File:    swi/settings.pl
    Author:  Roy Ratcliffe
    Created: Jun 15 2021
    Purpose: SWI Settings

Copyright (c) 2021, Roy Ratcliffe, Northumberland, United Kingdom

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

:- module(swi_settings,
          [ local_settings_file/1,              % -LocalFile:atom
            setting/3                           % :Name,?Value,:Goal
          ]).
:- meta_predicate setting(:, ?, :).
:- ensure_loaded(library(settings)).

%!  local_settings_file(-LocalFile:atom) is semidet.
%
%   Breaks the module interface by asking for the current local settings
%   file from the settings module. The local_file/1 dynamic predicate
%   retains the path of the current local file based on loading. Loading
%   a new settings file using load_settings/1 pushes a **new** local
%   file without replacing the old one, so that the next save_settings/0
%   keeps saving to the original file.
%
%   @arg LocalFile is the absolute path of the local settings file to be
%   utilised by the next save_settings/1 predicate call.

local_settings_file(LocalFile) :- once(settings:local_file(LocalFile)).

%!  setting(:Name, ?Value, :Goal) is semidet.
%
%   Semi-deterministic version of setting/2. Succeeds only if Value
%   succeeds for Goal; fails otherwise. Calls Goal with Value.
%
%   Take the following example where you only want the setting
%   predicate to succeed when it does _not_ match the empty atom.
%
%       setting(http:public_host, A, \==(''))

setting(Name, Value, Goal) :- setting(Name, Value), call(Goal, Value).
