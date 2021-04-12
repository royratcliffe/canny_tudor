:- module(canny_arch,
          [ current_arch/1,
            current_arch_os/2,
            current_os/1
          ]).

%!  current_arch(?Arch:pair) is semidet.

current_arch(Arch) :-
    current_prolog_flag(arch, Arch0),
    atom_to_term(Arch0, Arch, []).

%!  current_arch_os(?Arch, ?OS) is semidet.

current_arch_os(Arch, OS) :- current_arch(Arch-OS).

%!  current_os(?OS) is semidet.

current_os(OS) :- current_arch_os(_, OS).
