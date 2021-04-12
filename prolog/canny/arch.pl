:- module(canny_arch,
          [ current_arch/1,
            current_arch_os/2,
            current_os/1
          ]).

%!  current_arch(?Arch:pair) is semidet.
%
%   Unifies Arch with the  current   host's  architecture  and operating
%   system. Usefully reads the pair as a  Prolog term with which you can
%   unify its component parts.
%
%   The Prolog `arch` flag  combines  both   the  architecture  and  the
%   operating system as a  dash-separated   pair.  The  predicate splits
%   these two components apart  by  reading   the  underlying  atom as a
%   Prolog term. This makes an assumption about the format of the `arch`
%   flag.

current_arch(Arch) :-
    current_prolog_flag(arch, Arch0),
    atom_to_term(Arch0, Arch, []).

%!  current_arch_os(?Arch, ?OS) is semidet.
%
%   Unifies OS with the  current  operating   system.  Splits  the  host
%   architecture into its two components:  the bit-wise sub-architecture
%   and the operating system. Operating  system   is  one of: `win32` or
%   `win64` for Windows, `darwin` for macOS,  or `linux` for Linux. Maps
%   architecture bit-width to an  atomic   Arch  token  for contemporary
%   64-bit hosts, one of: `x64`, `x86_64`.   Darwin and Linux report the
%   latter, Windows the former.

current_arch_os(Arch, OS) :- current_arch(Arch-OS).

%!  current_os(?OS) is semidet.

current_os(OS) :- current_arch_os(_, OS).
