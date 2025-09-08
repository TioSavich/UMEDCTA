% Filename: incompatibility_semantics.pl (Complete, Safe, and Unified)
:- module(incompatibility_semantics,
          [ proves/1, obj_coll/1, incoherent/1, set_domain/1, current_domain/1
          , s/1, o/1, n/1, comp_nec/1, exp_nec/1, exp_poss/1, neg/1
          , highlander/2, bounded_region/4, equality_iterator/3
          % Exporting geometric vocabulary for testing
          , square/1, rectangle/1, rhombus/1, parallelogram/1, trapezoid/1, kite/1, quadrilateral/1
          , r1/1, r2/1, r3/1, r4/1, r5/1, r6/1
          ]).
% Declare predicates that are defined across different sections.
:- discontiguous proves_impl/2.
:- discontiguous is_incoherent/1. % Non-recursive check

% =================================================================
% Part 0: Setup and Configuration
% =================================================================

% Define operators for modalities, negation, and sequents.
:- op(500, fx, comp_nec).
:- op(500, fx, exp_nec).
:- op(500, fx, exp_poss).
:- op(500, fx, neg).
:- op(1050, xfy, =>).

% =================================================================
% Part 1: Knowledge Domains (Geometry, Arithmetic)
% =================================================================

% --- 1.1 Geometry (Chapter 2) ---

% Encoding Table 4 (PDF p. 28).
% R1: No sides equal
incompatible_pair(square, r1). incompatible_pair(rectangle, r1).
incompatible_pair(rhombus, r1). incompatible_pair(parallelogram, r1).
incompatible_pair(kite, r1).
% R2: No adjacent sides equal
incompatible_pair(square, r2). incompatible_pair(rhombus, r2).
incompatible_pair(kite, r2).
% R3: No opposite sides equal
incompatible_pair(square, r3). incompatible_pair(rectangle, r3).
incompatible_pair(rhombus, r3). incompatible_pair(parallelogram, r3).
% R4: Non-parallel sides not congruent
incompatible_pair(square, r4). incompatible_pair(rhombus, r4).
incompatible_pair(kite, r4).
% R5: No opposite sides parallel
incompatible_pair(square, r5). incompatible_pair(rectangle, r5).
incompatible_pair(rhombus, r5). incompatible_pair(parallelogram, r5).
incompatible_pair(trapezoid, r5).
% R6: No right angles
incompatible_pair(square, r6). incompatible_pair(rectangle, r6).

% Helper for identifying shapes.
is_shape(S) :- incompatible_pair(S, _), !.
is_shape(quadrilateral).

% Entailment via Incompatibility (Inferential Strength)
entails_via_incompatibility(P_shape, Q_shape) :-
    P_shape == Q_shape, !.
entails_via_incompatibility(_P_shape, quadrilateral) :- !.
entails_via_incompatibility(P_shape, Q_shape) :-
    % Check that for all R incompatible with Q, R is also incompatible with P.
    forall(incompatible_pair(Q_shape, R), incompatible_pair(P_shape, R)).

% List of all geometric predicates to exclude from arithmetic evaluation.
geometric_predicates([square, rectangle, rhombus, parallelogram, trapezoid, kite, quadrilateral, r1, r2, r3, r4, r5, r6]).

% --- 1.2 Arithmetic (O/N Domains) ---

:- dynamic current_domain/1.
current_domain(n). % Default arithmetic domain

set_domain(D) :-
    ( (D = n ; D = z) -> retractall(current_domain(_)), assertz(current_domain(D)) ; true).

% Domain-dependent Object Collection
obj_coll(N) :- current_domain(n), !, integer(N), N >= 0.
obj_coll(N) :- current_domain(z), !, integer(N).

% =================================================================
% Part 2: Core Logic Engine
% =================================================================

% Helper predicates
select(X, [X|T], T).
select(X, [H|T], [H|R]) :- select(X, T, R).

% --- 2.1 Incoherence Definitions (SAFE AND COMPLETE) ---

% Full definition of Incoherence (Restored).
% A set is incoherent if it is base incoherent OR if it proves the empty conclusion.
incoherent(X) :- is_incoherent(X), !.
incoherent(X) :- proves(X => []).

% is_incoherent/1: Non-recursive Incoherence Check (Used by the prover to avoid loops)

% --- 1. Specific Material Optimizations ---

% Geometric Incompatibility
is_incoherent(X) :-
    % Check if the set X contains an incompatible Shape/Restriction pair for the same variable V.
    member(n(ShapePred), X), ShapePred =.. [Shape, V],
    member(n(RestrictionPred), X), RestrictionPred =.. [Restriction, V],
    % Ensure names are ground for fast lookup.
    ground(Shape), ground(Restriction),
    incompatible_pair(Shape, Restriction),
    !.

% Arithmetic Incompatibility
is_incoherent(X) :-
    member(n(obj_coll(minus(A,B,_))), X),
    current_domain(n),
    number(A), number(B),
    A < B,
    !.

% --- 2. Base Incoherence (LNC) and Persistence ---

% Law of Non-Contradiction (LNC)
incoherent_base(X) :- member(P, X), member(neg(P), X).
% Generalized LNC across domains (s/o/n)
incoherent_base(X) :- member(D_P, X), D_P =.. [D, P], member(D_NegP, X), D_NegP =.. [D, neg(P)], member(D, [s,o,n]).

% Persistence
is_incoherent(Y) :- incoherent_base(Y), !.


% --- 2.2 Sequent Calculus Prover ---

proves(Sequent) :- proves_impl(Sequent, []).

% Axiom of Identity (A |- A)
proves_impl((Premises => Conclusions), _) :-
    member(P, Premises), member(P, Conclusions), !.

% From base incoherence (Explosion) - Uses the non-recursive check.
proves_impl((Premises => _), _) :-
    is_incoherent(Premises), !.

% --- Structural Rules (Domain Specific) ---

% Geometric Entailment (Inferential Strength)
proves_impl((Premises => Conclusions), _) :-
    % 1. Identify the goal: Proving n(P(X)) |- n(Q(X)).
    member(n(P_pred), Premises),
    P_pred =.. [P_shape, X],
    is_shape(P_shape),

    member(n(Q_pred), Conclusions),
    Q_pred =.. [Q_shape, X],
    is_shape(Q_shape),

    % 2. Check inferential strength.
    entails_via_incompatibility(P_shape, Q_shape), !.

% Arithmetic Evaluation
proves_impl(([Premise|RestPremises] => Conclusions), History) :-
    % Handle indexed (s/o/n) or raw expressions.
    (Premise =.. [Index, Expr], member(Index, [s, o, n]) ; (Index = none, Expr = Premise)),
    % Check exclusions to prevent evaluating logical/geometric terms
    (compound(Expr) -> (
        functor(Expr, F, _),
        geometric_predicates(GeoPreds),
        \+ member(F, [neg, conj, nec, comp_nec, exp_nec, exp_poss, obj_coll|GeoPreds])
    ) ; true),
    catch(Value is Expr, _, fail),
    !,
    (Index \= none -> NewPremise =.. [Index, Value] ; NewPremise = Value),
    proves_impl(([NewPremise|RestPremises] => Conclusions), History).

% Structural Rule for EML Dynamics
proves_impl((Premises => Conclusions), History) :-
    select(s(P), Premises, RestPremises),
    \+ member(s(P), History),
    eml_axiom(s(P), s(M_Q)),
    (   (M_Q = comp_nec Q ; M_Q = exp_nec Q)
    ->  proves_impl(([s(Q)|RestPremises] => Conclusions), [s(P)|History])
    ;   ((M_Q = exp_poss _), (member(s(M_Q), Conclusions) ; member(M_Q, Conclusions)))
    ).

% Helper for EML Dynamics
eml_axiom(Antecedent, Consequent) :-
    current_module(incompatibility_semantics),
    % Must specify the module when using clause/2 on module predicates.
    clause(incompatibility_semantics:proves_impl(([Antecedent] => [Consequent]), _), true),
    is_eml_modality(Consequent).

is_eml_modality(s(comp_nec _)).
is_eml_modality(s(exp_nec _)).
is_eml_modality(s(exp_poss _)).

% --- Reduction Schemata (Logical Connectives) ---

% Left Negation (LN)
proves_impl((P => C), H) :- select(neg(X), P, P1), !, proves_impl((P1 => [X|C]), H).
proves_impl((P => C), H) :- select(D_NegX, P, P1), D_NegX=..[D, neg(X)], member(D,[s,o,n]), !, D_X=..[D, X], proves_impl((P1 => [D_X|C]), H).

% Right Negation (RN)
proves_impl((P => C), H) :- select(neg(X), C, C1), !, proves_impl(([X|P] => C1), H).
proves_impl((P => C), H) :- select(D_NegX, C, C1), D_NegX=..[D, neg(X)], member(D,[s,o,n]), !, D_X=..[D, X], proves_impl(([D_X|P] => C1), H).

% Conjunction and S5 Modal rules (Generalized)
proves_impl((P => C), H) :- select(conj(X,Y), P, P1), !, proves_impl(([X,Y|P1] => C), H).
proves_impl((P => C), H) :- select(s(conj(X,Y)), P, P1), !, proves_impl(([s(X),s(Y)|P1] => C), H).

proves_impl((P => C), H) :- select(conj(X,Y), C, C1), !, proves_impl((P => [X|C1]), H), proves_impl((P => [Y|C1]), H).
proves_impl((P => C), H) :- select(s(conj(X,Y)), C, C1), !, proves_impl((P => [s(X)|C1]), H), proves_impl((P => [s(Y)|C1]), H).

proves_impl((P => C), H) :- select(nec(X), P, P1), !, ( proves_impl((P1 => C), H) ; \+ proves_impl(([] => [X]), []) ).
proves_impl((P => C), H) :- select(nec(X), C, C1), !, ( proves_impl((P => C1), H) ; proves_impl(([] => [X]), []) ).


% =================================================================
% Part 3: Material Inferences and Grounding (Axioms)
% =================================================================

% --- 3.1 Arithmetic Grounding ---
proves_impl(_ => [o(eq(A,B))], _) :- obj_coll(A), obj_coll(B), A =:= B.
proves_impl(_ => [o(plus(A,B,C))], _) :- obj_coll(A), obj_coll(B), C is A + B, obj_coll(C).
proves_impl(_ => [o(minus(A,B,C))], _) :-
    current_domain(D), obj_coll(A), obj_coll(B),
    ((D=n, A >= B) ; D=z), C is A - B.

% --- 3.2 Arithmetic Material Inferences ---
proves_impl([n(plus(A,B,C))] => [n(plus(B,A,C))], _).

% --- 3.3 EML Material Inferences (Axioms) ---
proves_impl([s(u)] => [s(comp_nec a)], _).
proves_impl([s(u_prime)] => [s(comp_nec a)], _).
proves_impl([s(a)] => [s(exp_poss lg)], _).
proves_impl([s(lg)] => [s(exp_nec u_prime)], _).
proves_impl([s(t_b)] => [s(comp_nec t_n)], _).
proves_impl([s(t_n)] => [s(comp_nec t_b)], _).

% =================================================================
% Part 4: Automata and Placeholders
% =================================================================
highlander([Result], Result) :- !.
highlander([], _) :- !, fail.
highlander([_|Rest], Result) :- highlander(Rest, Result).
bounded_region(I, L, U, R) :- ( number(I), I >= L, I =< U -> R = in_bounds(I) ; R = out_of_bounds(I) ).
equality_iterator(T, T, T) :- !.
equality_iterator(C, T, R) :- C < T, C1 is C + 1, equality_iterator(C1, T, R).

% Placeholder definitions for exported functors (Required by SWI-Prolog modules)
s(_). o(_). n(_). neg(_).
comp_nec(_). exp_nec(_). exp_poss(_).
square(_). rectangle(_). rhombus(_). parallelogram(_). trapezoid(_). kite(_). quadrilateral(_).
r1(_). r2(_). r3(_). r4(_). r5(_). r6(_).