:- module(os_search_paths,
          [   search_path_prepend/2,    % +Name:atom, +Directory:atom
              search_path/2,            % +Name:atom, ?Directories:list(atom)
              search_path_separator/1   % ?Separator:atom
          ]).

%!  search_path_prepend(+Name:atom, +Directory:atom) is det.
%
%   Adds Directory to a search-path environment variable. Note, this is
%   not naturally an atomic operation but the prepend makes it thread
%   safe by wrapping the fetching and storing within a mutex.
%
%   Prepends Directory to the environment search path by Name, unless
%   already present. Uses semi-colon as the search-path separator on
%   Windows operating systems, or colon everywhere else. Adds Directory
%   to the start of an existing path. Makes Directory the first and only
%   directory element if the search path does not yet exist.
%
%   Note that Directory should be an operating-system compatible search
%   path because non-Prolog software needs to search using the included
%   directory paths. Automatically converts incoming directory paths to
%   operating-system compatible paths.
%
%   Note also, the environment variable Name is case insensitive on
%   Windows, but not so on Unix-based operating systems.

search_path_prepend(Name, Directory0) :-
    prolog_to_os_filename(Directory0, Directory),
    with_mutex(
        search_path_prepend,
        (   search_path(Name, Directories)
        ->  (   memberchk(Directory, Directories)
            ->  true
            ;   search_path(Name, [Directory|Directories])
            )
        ;   setenv(Name, Directory)
        )).

%!  search_path(+Name:atom, -Directories:list(atom)) is semidet.
%!  search_path(+Name:atom, +Directories:list(atom)) is det.
%
%   Only fails if the environment does *not* contain the given
%   search-path variable. Does not fail if the variable does *not*
%   identify a proper separator-delimited variable.

search_path(Name, Directories) :-
    var(Directories),
    !,
    getenv(Name, Value),
    search_path_separator(Separator),
    atomic_list_concat(Directories, Separator, Value).
search_path(Name, Directories) :-
    search_path_separator(Separator),
    atomic_list_concat(Directories, Separator, Value),
    setenv(Name, Value).

%!  search_path_separator(?Separator:atom) is semidet.
%
%   Separator used for search paths: semi-colon on the Microsoft Windows
%   operating system; colon elsewhere.

search_path_separator((;)) :-
    current_prolog_flag(windows, true),
    !.
search_path_separator(:).
