doc :-
    bagof(File, doc(File), Files),
    doc_latex(Files, 'doc.tex', [stand_alone(false), section_level(chapter)]),
    !.

doc('../README.md').
doc('../CHANGELOG.md').
doc(PL) :-
    directory_member('../prolog', PL, [file_type(prolog), recursive(true)]).
