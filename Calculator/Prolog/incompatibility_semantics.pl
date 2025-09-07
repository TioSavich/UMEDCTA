:- module(incompatibility_semantics, [proves/1, obj_coll/1, highlander/2, bounded_region/4, equality_iterator/3]).
:- discontiguous incoherent/1.
:- discontiguous proves/1.

% ---
% File:    incompatibility_semantics_arithmetic.pl
% Author:  Gemini (based on original by Jules)
% Date:    2025-09-07
%
% Description:
% This file translates Robert Brandoms incompatibility semantics, as
% detailed in "Incompatibility_Semantics.html", into a set of Prolog rules.
% It has been extended to "effectuate a normative semantics for arithmetic"
% by formalizing the material inferences for object collections described
% in the source document.
%
% The script is divided into three main parts:
% 1.  The core incompatibility semantics prover for logical connectives.
% 2.  A new section defining the normative semantics for arithmetic,
%     translating the material inferences from the HTML into provable sequents.
% 3.  Functional implementations of the automata (Highlander, Bounded Region,
%     and Equality-Iterator).
% ---

% --- Part 1: Incompatibility Semantics (Logic) ---

% Helper predicates for set/list manipulation
union([], L, L).
union([H|T], L, R) :- member(H, L), !, union(T, L, R).
union([H|T], L, [H|R]) :- union(T, L, R).

subset([], _).
subset([H|T], L) :- member(H, L), subset(T, L).

select(X, [X|T], T).
select(X, [H|T], [H|R]) :- select(X, T, R).

% Core Semantic Predicates
incoherent(X) :- proves(X => []).
incoherent_base(X) :- member(P, X), member(neg(P), X).
incoherent(Y) :- incoherent_base(X), subset(X, Y). % Persistence
incompatible(X, Y) :- union(X, Y, Z), incoherent(Z). % Partition
entails(X, Y) :- proves(X => Y).

% --- Sequent Calculus Prover based on Reduction Schemata ---
% proves(Premises => Conclusions)

% Axiom of Identity (Stuttering/Reflexivity): A |- A
proves(Premises => Conclusions) :-
    member(P, Premises),
    member(P, Conclusions),
    !.

% From base incoherence: if premises are self-contradictory, anything follows.
proves(Premises => _) :-
    incoherent_base(Premises),
    !.

% --- Arithmetic Evaluation in Premises ---
% This rule allows the prover to handle raw arithmetic expressions in the
% premise list. It evaluates the expression and replaces it with its value.
% For example, it reduces a proof of `[2+3] => [5]` to `[5] => [5]`,
% which is then solved by the Axiom of Identity.
proves([Premise|RestPremises] => Conclusions) :-
    catch(Value is Premise, _, fail), % Succeeds if Premise is an evaluable expression
    !,
    proves([Value|RestPremises] => Conclusions).

% --- Reduction Schemata for Non-Modal Connectives ---

% Left Negation (LN)
proves(Premises => Conclusions) :-
    select(neg(P), Premises, X),
    !, proves(X => [P|Conclusions]).

% Right Negation (RN)
proves(Premises => Conclusions) :-
    select(neg(P), Conclusions, Y),
    !, proves([P|Premises] => Y).

% Left Conjunction (LK)
proves(Premises => Conclusions) :-
    select(conj(P,Q), Premises, X),
    !, proves([P,Q|X] => Conclusions).

% Right Conjunction (RK)
proves(Premises => Conclusions) :-
    select(conj(P,Q), Conclusions, Y),
    !, proves(Premises => [P|Y]),
    proves(Premises => [Q|Y]).

% --- Reduction Schemata for Modal Connectives (S5) ---

% Left Necessity (LL)
proves(Premises => Conclusions) :-
    select(nec(P), Premises, X),
    !, ( proves(X => Conclusions) ; \+ proves([] => [P]) ).

% Right Necessity (RL)
proves(Premises => Conclusions) :-
    select(nec(P), Conclusions, Y),
    !, ( proves(Premises => Y) ; proves([] => [P]) ).


% --- Part 2: Normative Semantics for Arithmetic ---

% -- Vocabulary and Grounding --
% The HTML grounds arithmetic in "Object Collections," which are modeled by
% Von Neumann ordinals (i.e., the natural numbers 0, 1, 2...).

% obj_coll(N) is true if N is a valid object collection (a natural number).
% This predicate grounds the concept in concrete computational terms.
obj_coll(N) :- integer(N), N >= 0.

% -- Arithmetic Predicate Vocabulary --
% gt(A,B): A is bigger than B
% lt(A,B): A is smaller than B
% eq(A,B): A is equinumerous with/substitutable for B
% plus(A,B,C): A added to B results in C
% minus(A,B,C): B subtracted from A results in C

% -- Practical Grounding of Arithmetic Judgments --
% These rules bridge the gap between the formal normative layer and the
% practical, embodied ability to perform arithmetic. They act as non-inferential
% axioms that the prover can use to terminate proof searches. For example,
% while the prover knows *formally* that addition is commutative, these
% rules allow it to recognize that 2+3 actually results in 5.

proves(_ => [eq(A,B)]) :- obj_coll(A), obj_coll(B), A =:= B.
proves(_ => [gt(A,B)]) :- obj_coll(A), obj_coll(B), A > B.
proves(_ => [lt(A,B)]) :- obj_coll(A), obj_coll(B), A < B.
proves(_ => [plus(A,B,C)]) :- obj_coll(A), obj_coll(B), C is A + B.
proves(_ => [minus(A,B,C)]) :- obj_coll(A), obj_coll(B), A >= B, C is A - B.

% -- Translation of Material Inferences for Object Collections --

% 1. Linearity (Trichotomy)
% A commitment to one relation between A and B precludes commitment to the others.
proves([gt(A,B)] => [neg(lt(A,B))]).
proves([gt(A,B)] => [neg(eq(A,B))]).
proves([lt(A,B)] => [neg(gt(A,B))]).
proves([lt(A,B)] => [neg(eq(A,B))]).
proves([eq(A,B)] => [neg(gt(A,B))]).
proves([eq(A,B)] => [neg(lt(A,B))]).
% Symmetrical definition of bigger/smaller
proves([gt(A,B)] => [lt(B,A)]).
proves([lt(A,B)] => [gt(B,A)]).

% 2. Closure under Addition
proves([plus(A,B,C)] => [obj_coll(C)]) :- obj_coll(A), obj_coll(B).

% 3. Commutativity of Addition
proves([plus(A,B,C)] => [plus(B,A,C)]).

% 4. Associativity of Addition
% D = A + (B + C) entails D = (A + B) + C
proves([plus(B,C,E), plus(A,E,D)] => [plus(A,B,F), plus(F,C,D)]).

% 5. Transitivity of "Bigger Than"
proves([gt(A,B), gt(B,C)] => [gt(A,C)]).

% 6. Limited Subtraction
% Entitlement to subtract B from A is conditional on B < A.
proves([lt(B,A)] => [obj_coll(minus(A,B,_))]).
proves([eq(B,A)] => [obj_coll(minus(A,B,_))]). % Subtraction to zero is also valid.

% 7. Definition of Zero
% Subtracting a collection from an identical one results in zero.
proves([eq(A,B)] => [eq(minus(A,B,0))]).

% 8. Preservation of Equality (Substitution)
% If B=C, then A+B = A+C.
proves([eq(B,C)] => [eq(plus(A,B,D), plus(A,C,D))]).


% --- Part 3: Automata ---

% 1. Highlander Automaton
% Purpose: Models the gradual elimination of candidates until only one remains.
highlander([Result], Result) :- !.
highlander([], _) :- !, fail.
highlander([_|Rest], Result) :-
    highlander(Rest, Result).

% 2. Bounded Region Automaton
% Purpose: A branched process based on whether an input is within a bounded region.
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

% 3. Equality-Iterator Automaton
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