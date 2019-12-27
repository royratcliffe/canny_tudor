:- module(swi_streams, [close_streams/2]).

%!  close_streams(+Streams:list, -Catchers:list) is det.
%
%   Closes zero or more Streams  while   accumulating  any exceptions at
%   Catchers.

close_streams(Streams, Catchers) :-
    phrase(close_streams(Streams), Catchers).

close_streams([]) -->
    [].
close_streams([Stream|Streams]) -->
    {   catch(close(Stream), Catcher, true)
    },
    (   {   var(Catcher)
        }
    ->  []
    ;   [Catcher]
    ),
    close_streams(Streams).
