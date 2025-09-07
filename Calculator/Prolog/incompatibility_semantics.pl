% Incompatibility Semantics, Reduction Schemata, and Automata in Prolog
% Author: Jules
% Date: 2025-09-06
%
% This file translates the incompatibility semantics and automata from
% "Incompatibility_Semantics.html" into a set of Prolog rules.
%
% Part 1: Incompatibility Semantics
% The core of this part is a sequent calculus prover based on the
% reduction schemata provided in the document.
%
% Part 2: Automata
% This part provides functional Prolog implementations for the three automata
% described in the document: Highlander, Bounded Region, and Equality-Iterator.

% --- Part 1: Incompatibility Semantics ---

% --- Helper Predicates for Set/List Manipulation ---

% union(A, B, C) is true if C is the union of lists A and B (order not preserved, duplicates removed from A).
union([], L, L).
union([H|T], L, R) :-
    member(H, L),
    !, union(T, L, R).
union([H|T], L, [H|R]) :-
    union(T, L, R).

% subset(A, B) is true if all elements of list A are present in list B.
subset([], _).
subset([H|T], L) :-
    member(H, L),
    subset(T, L).

% select(Elem, List, Rest) is true if Elem is in List and Rest is the list without one occurrence of Elem.
select(X, [X|T], T).
select(X, [H|T], [H|R]) :- select(X, T, R).


% --- Core Semantic Predicates ---

% incoherent(X) is true if the set of sentences X is incoherent.
% A set is incoherent if it entails a contradiction (entails an empty set).
% The base case for incoherence is a set containing a proposition and its negation.
incoherent(X) :- proves(X => []).

% A fundamental incoherence.
incoherent_base(X) :-
    member(P, X),
    member(neg(P), X).

% Axiom: Persistence
% If a set is incoherent, any superset is also incoherent.
incoherent(Y) :-
    incoherent_base(X),
    subset(X, Y).

% Axiom: Partition
% Two sets X and Y are incompatible iff their union is incoherent.
incompatible(X, Y) :-
    union(X, Y, Z),
    incoherent(Z).

% entails(X, Y) is a convenience wrapper for the prover.
entails(X, Y) :- proves(X => Y).


% --- Sequent Calculus Prover based on Reduction Schemata ---
% proves(Premises => Conclusions)

% Axiom of Identity: A |- A
% A sequent is proven if a formula exists in both the premises and conclusions.
proves(Premises => Conclusions) :-
    member(P, Premises),
    member(P, Conclusions),
    !.

% From base incoherence: if premises are self-contradictory, anything follows.
proves(Premises => _) :-
    incoherent_base(Premises),
    !.

% --- Reduction Schemata for Non-Modal Connectives ---

% Left Negation (LN): (X, Np |- Y) is provable if (X |- Y, p) is provable.
proves(Premises => Conclusions) :-
    select(neg(P), Premises, X),
    !, proves(X => [P|Conclusions]).

% Right Negation (RN): (X |- Y, Np) is provable if (X, p |- Y) is provable.
proves(Premises => Conclusions) :-
    select(neg(P), Conclusions, Y),
    !, proves([P|Premises] => Y).

% Left Conjunction (LK): (X, Kpq |- Y) is provable if (X, p, q |- Y) is provable.
proves(Premises => Conclusions) :-
    select(conj(P,Q), Premises, X),
    !, proves([P,Q|X] => Conclusions).

% Right Conjunction (RK): (X |- Y, Kpq) is provable if (X |- Y, p) and (X |- Y, q) are provable.
proves(Premises => Conclusions) :-
    select(conj(P,Q), Conclusions, Y),
    !, proves(Premises => [P|Y]),
    proves(Premises => [Q|Y]).

% --- Reduction Schemata for Modal Connectives (S5) ---

% Left Necessity (LL): (X, Lp |- Y) is provable if (X |- Y) is provable or (p is not a tautology).
% p is not a tautology if `[] => [p]` is not provable.
proves(Premises => Conclusions) :-
    select(nec(P), Premises, X),
    !,
    (   proves(X => Conclusions)
    ;   \+ proves([] => [P])
    ).

% Right Necessity (RL): (X |- Y, Lp) is provable if (X |- Y) is provable or (p is a tautology).
% p is a tautology if `[] => [p]` is provable.
proves(Premises => Conclusions) :-
    select(nec(P), Conclusions, Y),
    !,
    (   proves(Premises => Y)
    ;   proves([] => [P])
    ).

% --- Part 2: Automata ---

% --- 1. Highlander Automaton ---

% Purpose: Models the gradual elimination of candidates from a set until only one remains.
highlander([Result], Result) :- !.
highlander([], _) :- !, fail.
highlander([_|Rest], Result) :-
    highlander(Rest, Result).

% --- 2. Bounded Region Automaton ---

% Purpose: A branched process based on whether an input falls within a bounded region.
bounded_region(Input, Lower, Upper, Result) :-
    (   number(Input), number(Lower), number(Upper) ->
        ( Input >= Lower, Input =< Upper ->
            Result = in_bounds(Input)
        ;
            Result = out_of_bounds(Input)
        )
    ;
        error(type_error(number, [Input, Lower, Upper]), context(bounded_region/4, 'Inputs must be numbers.'))
    ).

% --- 3. Equality-Iterator Automaton ---

% Purpose: Iterates by comparing a current value to a target, updating until they are equal.
equality_iterator(Target, Target, Target) :- !.
equality_iterator(Current, Target, Result) :-
    Current < Target,
    !,
    NewCurrent is Current + 1,
    equality_iterator(NewCurrent, Target, Result).
equality_iterator(Current, Target, _) :-
    Current > Target,
    !, fail.
