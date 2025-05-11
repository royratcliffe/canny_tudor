%!  directory_entry(+Directory, ?Entry)// is nondet.
%
%   Neatly traverses a file system using a grammar.

directory_entry(Directory, Entry) -->
    { exists_directory(Directory),
      !,
      directory_entry(Directory, Entry_),
      entries_entry([Directory, Entry_], Directory_)
    },
    [Entry_],
    directory_entry(Directory_, Entry).
directory_entry(Directory, Entry, [], Tail) :-
    entries_entry([Directory|Tail], Entry).

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
