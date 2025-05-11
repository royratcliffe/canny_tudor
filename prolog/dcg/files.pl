/*  File:    dcg/files.pl
    Author:  Roy Ratcliffe
    Created: May 11 2025
    Purpose: Neat Filesystem Traversal by DCG

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

:- module(files,
          [ directory_entry//2,                  % +Directory, ?Entry
            directory_entry/2                    % +Directory, ?Entry
          ]).
:- autoload(library(lists), [member/2]).

%!  directory_entry(+Directory, ?Entry)// is nondet.
%
%   Neatly traverses a file system using a grammar.
%
%   Finds files and skips the special dot entries. Here, Entry refers
%   to a file. The grammar recursively traverses sub-directories beneath
%   the given Directory and yields every existing file path at
%   Entry. The directory acts as the root of the scan; it joins with
%   the entry to yield the full path of the file, but **not** with the
%   difference list. The second `List` argument of phrase/2 unifies
%   with a list of the corresponding sub-path components *without* the
%   root. The caller sees the full path *and* the relative
%   sub-components.

directory_entry(Directory, Entry) -->
    { exists_directory(Directory),
      !,
      directory_entry(Directory, Entry_),
      entries_entry([Directory, Entry_], Directory_)
    },
    [Entry_],
    directory_entry(Directory_, Entry).
directory_entry(Directory, Entry, [], Entries) :-
    entries_entry([Directory|Entries], Entry).

entries_entry(Entries, Entry) :- atomic_list_concat(Entries, /, Entry).

%!  directory_entry(+Directory, ?Entry) is nondet.
%
%   No need to check if the Entry exists. It does exist at the time of
%   directory iteration. That could easily change by deleting, moving or
%   renaming the entry.

directory_entry(Directory, Entry) :-
    directory_files(Directory, Entries),
    member(Entry, Entries),
    \+ special(Entry).

special(.).
special(..).
