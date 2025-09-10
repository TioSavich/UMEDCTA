% Filename: incompatibility_semantics.pl (Corrected, Reordered, and Complete)
:- module(incompatibility_semantics,
          [ proves/1, obj_coll/1, incoherent/1, set_domain/1, current_domain/1
          , s/1, o/1, n/1, comp_nec/1, exp_nec/1, exp_poss/1, neg/1
          , highlander/2, bounded_region/4, equality_iterator/3
          % Geometry
          , square/1, rectangle/1, rhombus/1, parallelogram/1, trapezoid/1, kite/1, quadrilateral/1
          , r1/1, r2/1, r3/1, r4/1, r5/1, r6/1
          % Number Theory (Euclid)
          , prime/1, composite/1, divides/2, is_complete/1
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
% Part 1: Knowledge Domains
% =================================================================

% --- 1.1 Geometry (Chapter 2) ---
incompatible_pair(square, r1). incompatible_pair(rectangle, r1). incompatible_pair(rhombus, r1). incompatible_pair(parallelogram, r1). incompatible_pair(kite, r1).
incompatible_pair(square, r2). incompatible_pair(rhombus, r2). incompatible_pair(kite, r2).
incompatible_pair(square, r3). incompatible_pair(rectangle, r3). incompatible_pair(rhombus, r3). incompatible_pair(parallelogram, r3).
incompatible_pair(square, r4). incompatible_pair(rhombus, r4). incompatible_pair(kite, r4).
incompatible_pair(square, r5). incompatible_pair(rectangle, r5). incompatible_pair(rhombus, r5). incompatible_pair(parallelogram, r5). incompatible_pair(trapezoid, r5).
incompatible_pair(square, r6). incompatible_pair(rectangle, r6).

is_shape(S) :- (incompatible_pair(S, _); S = quadrilateral), !.

entails_via_incompatibility(P, Q) :- P == Q, !.
entails_via_incompatibility(_, quadrilateral) :- !.
entails_via_incompatibility(P, Q) :- forall(incompatible_pair(Q, R), incompatible_pair(P, R)).

geometric_predicates([square, rectangle, rhombus, parallelogram, trapezoid, kite, quadrilateral, r1, r2, r3, r4, r5, r6]).

% --- 1.2 Arithmetic (O/N Domains) ---

:- dynamic current_domain/1.
current_domain(n).

set_domain(D) :- ( (D = n ; D = z) -> retractall(current_domain(_)), assertz(current_domain(D)) ; true).

obj_coll(N) :- current_domain(n), !, integer(N), N >= 0.
obj_coll(N) :- current_domain(z), !, integer(N).

% --- 1.3 Number Theory Domain (Euclid) ---

number_theory_predicates([prime, composite, divides, is_complete, analyze_euclid_number, member]).

% Combined list of excluded predicates for Arithmetic Evaluation
excluded_predicates(AllPreds) :-
    geometric_predicates(G),
    number_theory_predicates(NT),
    append(G, NT, DomainPreds),
    append([neg, conj, nec, comp_nec, exp_nec, exp_poss, obj_coll], DomainPreds, AllPreds).

% --- Helpers for Number Theory (Grounded) ---

% Helper: Product of a list
product_of_list(L, P) :- (is_list(L) -> product_of_list_impl(L, P) ; fail).
product_of_list_impl([], 1).
product_of_list_impl([H|T], P) :- number(H), product_of_list_impl(T, P_tail), P is H * P_tail.

% Helper: Find a prime factor
find_prime_factor(N, F) :- number(N), N > 1, find_factor_from(N, 2, F).
find_factor_from(N, D, D) :- N mod D =:= 0, !.
find_factor_from(N, D, F) :-
    D * D =< N,
    (D =:= 2 -> D_next is 3 ; D_next is D + 2),
    find_factor_from(N, D_next, F).
find_factor_from(N, _, N). % N is prime

% Helper: Grounded check for primality
is_prime(N) :- number(N), N > 1, find_factor_from(N, 2, F), F =:= N.

% =================================================================
% Part 2: Core Logic Engine
% =================================================================

% Helper predicates
select(X, [X|T], T).
select(X, [H|T], [H|R]) :- select(X, T, R).

% Helper to match antecedents against premises (Allows unification)
match_antecedents([], _).
match_antecedents([A|As], Premises) :-
    member(A, Premises),
    match_antecedents(As, Premises).

% --- 2.1 Incoherence Definitions (SAFE AND COMPLETE) ---

% Full definition of Incoherence.
incoherent(X) :- is_incoherent(X), !.
incoherent(X) :- proves(X => []).

% is_incoherent/1: Non-recursive Incoherence Check

% --- 1. Specific Material Optimizations ---

% Geometric Incompatibility
is_incoherent(X) :-
    member(n(ShapePred), X), ShapePred =.. [Shape, V],
    member(n(RestrictionPred), X), RestrictionPred =.. [Restriction, V],
    ground(Shape), ground(Restriction),
    incompatible_pair(Shape, Restriction), !.

% Arithmetic Incompatibility
is_incoherent(X) :-
    member(n(obj_coll(minus(A,B,_))), X),
    current_domain(n), number(A), number(B), A < B, !.

% M6-Case1: Euclid Case 1 Incoherence
is_incoherent(X) :-
    member(n(prime(EF)), X),
    member(n(is_complete(L)), X),
    product_of_list(L, DE),
    EF is DE + 1.

% --- 2. Base Incoherence (LNC) and Persistence ---

% Law of Non-Contradiction (LNC)
incoherent_base(X) :- member(P, X), member(neg(P), X).
incoherent_base(X) :- member(D_P, X), D_P =.. [D, P], member(D_NegP, X), D_NegP =.. [D, neg(P)], member(D, [s,o,n]).

% Persistence
is_incoherent(Y) :- incoherent_base(Y), !.


% --- 2.2 Sequent Calculus Prover (REORDERED) ---
% Order: Identity/Explosion -> Axioms -> Structural Rules -> Reduction Schemata.

proves(Sequent) :- proves_impl(Sequent, []).

% --- PRIORITY 1: Identity and Explosion ---

% Axiom of Identity (A |- A)
proves_impl((Premises => Conclusions), _) :-
    member(P, Premises), member(P, Conclusions), !.

% From base incoherence (Explosion)
proves_impl((Premises => _), _) :-
    is_incoherent(Premises), !.

% --- PRIORITY 2: Material Inferences and Grounding (Axioms) ---

% --- Arithmetic Grounding ---
proves_impl(_ => [o(eq(A,B))], _) :- obj_coll(A), obj_coll(B), A =:= B.
proves_impl(_ => [o(plus(A,B,C))], _) :- obj_coll(A), obj_coll(B), C is A + B, obj_coll(C).
proves_impl(_ => [o(minus(A,B,C))], _) :-
    current_domain(D), obj_coll(A), obj_coll(B),
    ((D=n, A >= B) ; D=z), C is A - B.

% --- Arithmetic Material Inferences ---
proves_impl([n(plus(A,B,C))] => [n(plus(B,A,C))], _).

% --- EML Material Inferences (Axioms) ---
proves_impl([s(u)] => [s(comp_nec a)], _).
proves_impl([s(u_prime)] => [s(comp_nec a)], _).
proves_impl([s(a)] => [s(exp_poss lg)], _).
proves_impl([s(lg)] => [s(exp_nec u_prime)], _).
proves_impl([s(t_b)] => [s(comp_nec t_n)], _).
proves_impl([s(t_n)] => [s(comp_nec t_b)], _).

% --- Number Theory Material Inferences ---

% M5-Revised: Euclid's Core Argument (For Forward Chaining)
% Context n(is_complete(L)) added so MMP can bind L from premises.
proves_impl(( [n(prime(G)), n(divides(G, N)), n(is_complete(L))] => [n(neg(member(G, L)))] ), _) :-
    product_of_list(L, P),
    N is P + 1.

% M5-Direct: (For Direct proof, where L is bound by the conclusion)
proves_impl(( [n(prime(G)), n(divides(G, N))] => [n(neg(member(G, L)))] ), _) :-
    product_of_list(L, P),
    N is P + 1.

% M4-Revised: Definition of Completeness Violation (For Forward Chaining)
proves_impl(([n(prime(G)), n(neg(member(G, L))), n(is_complete(L))] => [n(neg(is_complete(L)))]), _).

% M4-Direct: (For Direct proof)
proves_impl(([n(prime(G)), n(neg(member(G, L)))] => [n(neg(is_complete(L)))]), _).

% Grounding Primality
proves_impl(([] => [n(prime(N))]), _) :- is_prime(N).
proves_impl(([] => [n(composite(N))]), _) :- number(N), N > 1, \+ is_prime(N).


% --- PRIORITY 3: Structural Rules (Domain Specific and General) ---

% Geometric Entailment (Inferential Strength)
proves_impl((Premises => Conclusions), _) :-
    member(n(P_pred), Premises), P_pred =.. [P_shape, X], is_shape(P_shape),
    member(n(Q_pred), Conclusions), Q_pred =.. [Q_shape, X], is_shape(Q_shape),
    entails_via_incompatibility(P_shape, Q_shape), !.

% Structural Rule for EML Dynamics
proves_impl((Premises => Conclusions), History) :-
    select(s(P), Premises, RestPremises), \+ member(s(P), History),
    eml_axiom(s(P), s(M_Q)),
    ( (M_Q = comp_nec Q ; M_Q = exp_nec Q) -> proves_impl(([s(Q)|RestPremises] => Conclusions), [s(P)|History])
    ; ((M_Q = exp_poss _), (member(s(M_Q), Conclusions) ; member(M_Q, Conclusions)))
    ).

% --- Structural Rules for Euclid's Proof ---

% Structural Rule: Euclid's Construction
proves_impl((Premises => Conclusions), History) :-
    member(n(is_complete(L)), Premises),
    \+ member(euclid_construction(L), History),
    product_of_list(L, DE),
    EF is DE + 1,
    NewPremise = n(analyze_euclid_number(EF, L)),
    proves_impl(([NewPremise|Premises] => Conclusions), [euclid_construction(L)|History]).

% Case Analysis Rule (Handles analyze_euclid_number)
proves_impl((Premises => Conclusions), History) :-
    select(n(analyze_euclid_number(EF, L)), Premises, RestPremises),
    EF > 1,
    (member(n(is_complete(L)), Premises) ->
        % Case 1: Assume EF is prime
        proves_impl(([n(prime(EF))|RestPremises] => Conclusions), History),
        % Case 2: Assume EF is composite
        proves_impl(([n(composite(EF))|RestPremises] => Conclusions), History)
    ; fail
    ).

% Structural Rule: Prime Factorization (Existential Instantiation) (Case 2)
proves_impl((Premises => Conclusions), History) :-
    select(n(composite(N)), Premises, RestPremises),
    \+ member(factorization(N), History),
    find_prime_factor(N, G),
    NewPremises = [n(prime(G)), n(divides(G, N))|RestPremises],
    proves_impl((NewPremises => Conclusions), [factorization(N)|History]).

% --- General Structural Rule: Forward Chaining (Modus Ponens / MMP) --- (CORRECTED)
proves_impl((Premises => Conclusions), History) :-
    Module = incompatibility_semantics,
    % 1. Find an applicable material inference rule (axiom) defined in Priority 2.
    clause(Module:proves_impl((A_clause => [C_clause]), _), B_clause),
    
    copy_term((A_clause, C_clause, B_clause), (Antecedents, Consequent, Body)),
    is_list(Antecedents), % Handle grounding axioms like [] => P

    % 2. Check if the antecedents are satisfied by the current premises.
    match_antecedents(Antecedents, Premises),
    % 3. Execute the body of the axiom (FIX: ensures correct module context).
    call(Module:Body),
    % 4. Ensure the consequent hasn't already been derived.
    \+ member(Consequent, Premises),
    % 5. Add the consequent to the premises and continue.
    proves_impl(([Consequent|Premises] => Conclusions), History).


% Arithmetic Evaluation
proves_impl(([Premise|RestPremises] => Conclusions), History) :-
    (Premise =.. [Index, Expr], member(Index, [s, o, n]) ; (Index = none, Expr = Premise)),
    (compound(Expr) -> (
        functor(Expr, F, _),
        excluded_predicates(Excluded),
        \+ member(F, Excluded)
    ) ; true),
    catch(Value is Expr, _, fail), !,
    (Index \= none -> NewPremise =.. [Index, Value] ; NewPremise = Value),
    proves_impl(([NewPremise|RestPremises] => Conclusions), History).


% --- PRIORITY 4: Reduction Schemata (Logical Connectives) ---
% CRITICAL FIX: Removed cuts (!) to allow backtracking.

% Left Negation (LN)
proves_impl((P => C), H) :- select(neg(X), P, P1), proves_impl((P1 => [X|C]), H).
proves_impl((P => C), H) :- select(D_NegX, P, P1), D_NegX=..[D, neg(X)], member(D,[s,o,n]), D_X=..[D, X], proves_impl((P1 => [D_X|C]), H).

% Right Negation (RN)
proves_impl((P => C), H) :- select(neg(X), C, C1), proves_impl(([X|P] => C1), H).
proves_impl((P => C), H) :- select(D_NegX, C, C1), D_NegX=..[D, neg(X)], member(D,[s,o,n]), D_X=..[D, X], proves_impl(([D_X|P] => C1), H).

% Conjunction (Generalized)
proves_impl((P => C), H) :- select(conj(X,Y), P, P1), proves_impl(([X,Y|P1] => C), H).
proves_impl((P => C), H) :- select(s(conj(X,Y)), P, P1), proves_impl(([s(X),s(Y)|P1] => C), H).

proves_impl((P => C), H) :- select(conj(X,Y), C, C1), proves_impl((P => [X|C1]), H), proves_impl((P => [Y|C1]), H).
proves_impl((P => C), H) :- select(s(conj(X,Y)), C, C1), proves_impl((P => [s(X)|C1]), H), proves_impl((P => [s(Y)|C1]), H).

% S5 Modal rules (Generalized)
% Note: Cuts are necessary here for the S5 implementation.
proves_impl((P => C), H) :- select(nec(X), P, P1), !, ( proves_impl((P1 => C), H) ; \+ proves_impl(([] => [X]), []) ).
proves_impl((P => C), H) :- select(nec(X), C, C1), !, ( proves_impl((P => C1), H) ; proves_impl(([] => [X]), []) ).

% (Helpers for EML Dynamics)
eml_axiom(A, C) :-
    clause(incompatibility_semantics:proves_impl(([A] => [C]), _), true),
    is_eml_modality(C).
is_eml_modality(s(comp_nec _)). is_eml_modality(s(exp_nec _)). is_eml_modality(s(exp_poss _)).

% =================================================================
% Part 4: Automata and Placeholders
% =================================================================

% (FIX: Restored Automata definitions)
highlander([Result], Result) :- !.
highlander([], _) :- !, fail.
highlander([_|Rest], Result) :- highlander(Rest, Result).

bounded_region(I, L, U, R) :- ( number(I), I >= L, I =< U -> R = in_bounds(I) ; R = out_of_bounds(I) ).

equality_iterator(T, T, T) :- !.
equality_iterator(C, T, R) :- C < T, C1 is C + 1, equality_iterator(C1, T, R).

% Placeholder definitions for exported functors
s(_). o(_). n(_). neg(_). comp_nec(_). exp_nec(_). exp_poss(_).
square(_). rectangle(_). rhombus(_). parallelogram(_). trapezoid(_). kite(_). quadrilateral(_).
r1(_). r2(_). r3(_). r4(_). r5(_). r6(_).
prime(_). composite(_). divides(_, _). is_complete(_). analyze_euclid_number(_, _).