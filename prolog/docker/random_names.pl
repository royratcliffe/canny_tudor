:- module(docker_random_names,
          [   random_name/1,            % ?Name
              random_name_chk/1,        % -Name
              random_name_chk/2         % ?LHS, ?RHS
          ]).

%!  random_name(?Name) is nondet.
%
%   Non-deterministically  generates  Docker-style  random  names.  Uses
%   random_permutation/2 and member/2, rather   than random_member/2, in
%   order to generate all possible  random   names  by  back-tracking if
%   necessary.
%
%   The engine-based implementation has  two   key  features:  generates
%   random permutations of both left  and right sub-names independently;
%   does not repeat until after unifying  all permutations. This implies
%   that two consecutive names will  never  be   the  same  up until the
%   boundary event between two consecutive   randomisations.  There is a
%   possibility, albeit small, that  the  last   random  name  from  one
%   sequence might accidentally match the first  name in the next random
%   sequence. There are 23,500 possible combinations.
%
%   The implementation is *not* the  most   efficient,  but does perform
%   accurate randomisation over all left-right name permutations.
%
%   Allows Name to  collapse  to   semi-determinism  with  ground  terms
%   without continuous random-name generation since  it will never match
%   an atom that does not belong  to   the  Docker-random  name set. The
%   engine-based non-determinism only kicks in when Name unbound.

random_name(Name) :-
    var(Name),
    !,
    setup_call_cleanup(
        engine_create(_, random_name_, Engine),
        random_name_(Name, Engine),
        engine_destroy(Engine)
    ).
random_name(Name) :-
    lhs(LHS),
    rhs(RHS),
    atomic_list_concat([LHS, RHS], '_', Name),
    !.

random_name_ :-
    repeat,
    random_name_(Name),
    engine_yield(Name),
    fail.

random_name_(Name) :-
    findall(LHS-RHS, (lhs(LHS), rhs(RHS)), Names0),
    random_permutation(Names0, Names),
    member(LHS-RHS, Names),
    atomic_list_concat([LHS, RHS], '_', Name).

random_name_(Name, Engine) :-
    engine_next_reified(Engine, Term),
    random_name_(Term, Name, Engine).

random_name_(the(Name), Name, _).
random_name_(the(_), Name, Engine) :-
    !,
    engine_next_reified(Engine, Term),
    random_name_(Term, Name, Engine).
random_name_(exception(Catcher), _, _) :-
    throw(Catcher).
random_name_(no, _, _) :-
    fail.

%!  random_name_chk(-Name:atom) is det.
%
%   Generates a random Name.
%
%   Only ever fails if Name is bound and fails to match the next random
%   Name, without testing for an unbound argument. That makes little
%   sense, so fails unless Name is a variable.

random_name_chk(Name) :-
    var(Name),
    random_name_chk(LHS, RHS),
    atomic_list_concat([LHS, RHS], '_', Name).

%!  random_name_chk(?LHS:atom, ?RHS:atom) is semidet.
%
%   Unifies LHS-RHS with one random name, a randomised selection from
%   all possible names.
%
%   Note, this does *not* naturally work in (+, ?) or (?, +) or (+, +)
%   modes, even if required. Predicate random_member/2 fails
%   semi-deterministically if the given atom fails to match the
%   randomised selection. Unifies semi-deterministically for ground
%   atoms in order to work correctly for non-variable arguments. It
%   collapses to failure if the argument cannot unify with random-name
%   possibilities.

random_name_chk(LHS, RHS) :-
    random_name_chk_(LHS0, lhs(LHS0), LHS),
    random_name_chk_(RHS0, rhs(RHS0), RHS).

random_name_chk_(Template, Goal, Member) :-
    var(Member),
    !,
    findall(Template, Goal, Members),
    random_member(Member, Members).
random_name_chk_(Member, Goal, Member) :-
    Goal.

:- public
    lhs/1,
    rhs/1.

lhs(admiring).
lhs(adoring).
lhs(affectionate).
lhs(agitated).
lhs(amazing).
lhs(angry).
lhs(awesome).
lhs(blissful).
lhs(bold).
lhs(boring).
lhs(brave).
lhs(charming).
lhs(clever).
lhs(cocky).
lhs(cool).
lhs(compassionate).
lhs(competent).
lhs(condescending).
lhs(confident).
lhs(cranky).
lhs(crazy).
lhs(dazzling).
lhs(determined).
lhs(distracted).
lhs(dreamy).
lhs(eager).
lhs(ecstatic).
lhs(elastic).
lhs(elated).
lhs(elegant).
lhs(eloquent).
lhs(epic).
lhs(fervent).
lhs(festive).
lhs(flamboyant).
lhs(focused).
lhs(friendly).
lhs(frosty).
lhs(gallant).
lhs(gifted).
lhs(goofy).
lhs(gracious).
lhs(happy).
lhs(hardcore).
lhs(heuristic).
lhs(hopeful).
lhs(hungry).
lhs(infallible).
lhs(inspiring).
lhs(jolly).
lhs(jovial).
lhs(keen).
lhs(kind).
lhs(laughing).
lhs(loving).
lhs(lucid).
lhs(magical).
lhs(mystifying).
lhs(modest).
lhs(musing).
lhs(naughty).
lhs(nervous).
lhs(nifty).
lhs(nostalgic).
lhs(objective).
lhs(optimistic).
lhs(peaceful).
lhs(pedantic).
lhs(pensive).
lhs(practical).
lhs(priceless).
lhs(quirky).
lhs(quizzical).
lhs(recursing).
lhs(relaxed).
lhs(reverent).
lhs(romantic).
lhs(sad).
lhs(serene).
lhs(sharp).
lhs(silly).
lhs(sleepy).
lhs(stoic).
lhs(stupefied).
lhs(suspicious).
lhs(sweet).
lhs(tender).
lhs(thirsty).
lhs(trusting).
lhs(unruffled).
lhs(upbeat).
lhs(vibrant).
lhs(vigilant).
lhs(vigorous).
lhs(wizardly).
lhs(wonderful).
lhs(xenodochial).
lhs(youthful).
lhs(zealous).
lhs(zen).

rhs(albattani).
rhs(allen).
rhs(almeida).
rhs(antonelli).
rhs(agnesi).
rhs(archimedes).
rhs(ardinghelli).
rhs(aryabhata).
rhs(austin).
rhs(babbage).
rhs(banach).
rhs(banzai).
rhs(bardeen).
rhs(bartik).
rhs(bassi).
rhs(beaver).
rhs(bell).
rhs(benz).
rhs(bhabha).
rhs(bhaskara).
rhs(black).
rhs(blackburn).
rhs(blackwell).
rhs(bohr).
rhs(booth).
rhs(borg).
rhs(bose).
rhs(boyd).
rhs(brahmagupta).
rhs(brattain).
rhs(brown).
rhs(buck).
rhs(burnell).
rhs(cannon).
rhs(carson).
rhs(cartwright).
rhs(cerf).
rhs(chandrasekhar).
rhs(chaplygin).
rhs(chatelet).
rhs(chatterjee).
rhs(chebyshev).
rhs(cohen).
rhs(chaum).
rhs(clarke).
rhs(colden).
rhs(cori).
rhs(cray).
rhs(curran).
rhs(curie).
rhs(darwin).
rhs(davinci).
rhs(dewdney).
rhs(dhawan).
rhs(diffie).
rhs(dijkstra).
rhs(dirac).
rhs(driscoll).
rhs(dubinsky).
rhs(easley).
rhs(edison).
rhs(einstein).
rhs(elbakyan).
rhs(elgamal).
rhs(elion).
rhs(ellis).
rhs(engelbart).
rhs(euclid).
rhs(euler).
rhs(faraday).
rhs(feistel).
rhs(fermat).
rhs(fermi).
rhs(feynman).
rhs(franklin).
rhs(gagarin).
rhs(galileo).
rhs(galois).
rhs(ganguly).
rhs(gates).
rhs(gauss).
rhs(germain).
rhs(goldberg).
rhs(goldstine).
rhs(goldwasser).
rhs(golick).
rhs(goodall).
rhs(gould).
rhs(greider).
rhs(grothendieck).
rhs(haibt).
rhs(hamilton).
rhs(haslett).
rhs(hawking).
rhs(hellman).
rhs(heisenberg).
rhs(hermann).
rhs(herschel).
rhs(hertz).
rhs(heyrovsky).
rhs(hodgkin).
rhs(hofstadter).
rhs(hoover).
rhs(hopper).
rhs(hugle).
rhs(hypatia).
rhs(ishizaka).
rhs(jackson).
rhs(jang).
rhs(jennings).
rhs(jepsen).
rhs(johnson).
rhs(joliot).
rhs(jones).
rhs(kalam).
rhs(kapitsa).
rhs(kare).
rhs(keldysh).
rhs(keller).
rhs(kepler).
rhs(khayyam).
rhs(khorana).
rhs(kilby).
rhs(kirch).
rhs(knuth).
rhs(kowalevski).
rhs(lalande).
rhs(lamarr).
rhs(lamport).
rhs(leakey).
rhs(leavitt).
rhs(lederberg).
rhs(lehmann).
rhs(lewin).
rhs(lichterman).
rhs(liskov).
rhs(lovelace).
rhs(lumiere).
rhs(mahavira).
rhs(margulis).
rhs(matsumoto).
rhs(maxwell).
rhs(mayer).
rhs(mccarthy).
rhs(mcclintock).
rhs(mclaren).
rhs(mclean).
rhs(mcnulty).
rhs(mendel).
rhs(mendeleev).
rhs(meitner).
rhs(meninsky).
rhs(merkle).
rhs(mestorf).
rhs(minsky).
rhs(mirzakhani).
rhs(moore).
rhs(morse).
rhs(murdock).
rhs(moser).
rhs(napier).
rhs(nash).
rhs(neumann).
rhs(newton).
rhs(nightingale).
rhs(nobel).
rhs(noether).
rhs(northcutt).
rhs(noyce).
rhs(panini).
rhs(pare).
rhs(pascal).
rhs(pasteur).
rhs(payne).
rhs(perlman).
rhs(pike).
rhs(poincare).
rhs(poitras).
rhs(proskuriakova).
rhs(ptolemy).
rhs(raman).
rhs(ramanujan).
rhs(ride).
rhs(montalcini).
rhs(ritchie).
rhs(rhodes).
rhs(robinson).
rhs(roentgen).
rhs(rosalind).
rhs(rubin).
rhs(saha).
rhs(sammet).
rhs(sanderson).
rhs(shamir).
rhs(shannon).
rhs(shaw).
rhs(shirley).
rhs(shockley).
rhs(shtern).
rhs(sinoussi).
rhs(snyder).
rhs(solomon).
rhs(spence).
rhs(stallman).
rhs(stonebraker).
rhs(sutherland).
rhs(swanson).
rhs(swartz).
rhs(swirles).
rhs(taussig).
rhs(tereshkova).
rhs(tesla).
rhs(tharp).
rhs(thompson).
rhs(torvalds).
rhs(tu).
rhs(turing).
rhs(varahamihira).
rhs(vaughan).
rhs(visvesvaraya).
rhs(volhard).
rhs(villani).
rhs(wescoff).
rhs(wilbur).
rhs(wiles).
rhs(williams).
rhs(williamson).
rhs(wilson).
rhs(wing).
rhs(wozniak).
rhs(wright).
rhs(wu).
rhs(yalow).
rhs(yonath).
rhs(zhukovsky).
