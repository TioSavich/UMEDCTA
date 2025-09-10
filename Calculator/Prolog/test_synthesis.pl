% Filename: test_synthesis.pl (Corrected)
:- use_module(incompatibility_semantics).
:- use_module(library(plunit)).

% Ensure operators are visible
:- op(500, fx, neg).
:- op(1050, xfy, =>).

:- begin_tests(unified_synthesis).

% --- Tests for Part 1: Core Logic and Domains ---
test(identity_subjective) :- assertion(proves([s(p)] => [s(p)])).
test(incoherence_subjective) :- assertion(incoherent([s(p), s(neg(p))])).

test(negation_handling_subjective_lem) :-
    assertion(proves([] => [s(p), s(neg(p))])).

% --- Tests for Part 2: Arithmetic Coexistence and Fixes ---

test(arithmetic_commutativity_normative) :-
    assertion(proves([n(plus(2,3,5))] => [n(plus(3,2,5))])).

test(arithmetic_subtraction_limit_n, [setup(set_domain(n))]) :-
    assertion(incoherent([n(obj_coll(minus(3,5,_)))])).

test(arithmetic_subtraction_limit_n_persistence, [setup(set_domain(n))]) :-
    assertion(incoherent([n(obj_coll(minus(3,5,_))), s(p)])).

test(arithmetic_subtraction_limit_z, [setup(set_domain(z))]) :-
    assertion(\+(incoherent([n(obj_coll(minus(3,5,_)))]))).

% --- Tests for Part 3: Embodied Modal Logic (EML) ---
test(eml_dynamic_u_to_a) :- assertion(proves([s(u)] => [s(a)])).
test(eml_dynamic_full_cycle) :- assertion(proves([s(lg)] => [s(a)])).
test(hegel_loop_prevention) :-
    assertion(\+(proves([s(t_b)] => [s(x)]))).

% --- Tests for New Feature: Quadrilateral Hierarchy (Chapter 2) ---

test(quad_incompatibility_square_r1) :-
    assertion(incoherent([n(square(x)), n(r1(x))])).

test(quad_compatibility_trapezoid_r1) :-
    assertion(\+(incoherent([n(trapezoid(x)), n(r1(x))]))).

test(quad_incompatibility_persistence) :-
    assertion(incoherent([n(square(x)), n(r1(x)), s(other)])).

test(quad_entailment_square_rectangle) :-
    assertion(proves([n(square(x))] => [n(rectangle(x))])).

test(quad_entailment_rectangle_square_fail) :-
    assertion(\+(proves([n(rectangle(x))] => [n(square(x))]))).

test(quad_entailment_rhombus_kite) :-
    assertion(proves([n(rhombus(x))] => [n(kite(x))])).

test(quad_entailment_transitive) :-
    assertion(proves([n(square(x))] => [n(parallelogram(x))])).

test(quad_projection_contrapositive) :-
    assertion(proves([n(neg(rectangle(x)))] => [n(neg(square(x)))])).

test(quad_projection_inversion_fail) :-
    assertion(\+(proves([n(neg(square(x)))] => [n(neg(rectangle(x)))]))).

% --- Tests for Number Theory (Euclid's Proof) ---

% Test Grounding Helpers
test(euclid_grounding_prime) :-
    assertion(proves([] => [n(prime(7))])),
    assertion(\+ proves([] => [n(prime(6))])).

test(euclid_grounding_composite) :-
    assertion(proves([] => [n(composite(6))])),
    assertion(\+ proves([] => [n(composite(7))])).

% Test Material Inferences (M4 and M5)
test(euclid_material_inference_m5) :-
    % M5: If P divides Product(L)+1, P is not in L.
    % L=[2,3], Product(L)+1 = 7. P=7.
    % FIX: Ensure correct syntax neg(member(...)).
    assertion(proves([n(prime(7)), n(divides(7, 7))] => [n(neg(member(7, [2, 3])))])).

test(euclid_material_inference_m4) :-
    % M4: If P is prime and not in L, L is not complete.
    % FIX: Ensure correct syntax.
    assertion(proves([n(prime(5)), n(neg(member(5, [2, 3])))] => [n(neg(is_complete([2, 3])))] )).

% Test Forward Chaining (Combining M5 and M4)
test(euclid_forward_chaining) :-
    % Test MMP using the Revised axioms.
    % If P divides Product(L)+1 AND L is assumed complete, then L is not complete.
    % L=[2,3], N=7, P=7.
    Premises = [n(prime(7)), n(divides(7, 7)), n(is_complete([2, 3]))],
    % The conclusion requires M5-R to fire first, then M4-R.
    Conclusion = [n(neg(is_complete([2, 3])))],
    % This verifies the Forward Chaining structural rule works and handles conditions/bindings.
    assertion(proves(Premises => Conclusion)).

% Test Case 1 (N is Prime)
test(euclid_case_1_incoherence) :-
    % L=[2,3], N=7. If N is prime and L is complete, it is incoherent.
    % This verifies the specific optimization for Case 1 (M6-Case1).
    assertion(incoherent([n(prime(7)), n(is_complete([2, 3]))])).

% Test Case 2 (N is Composite)
test(euclid_case_2_incoherence) :-
    % L=[2,3,5,7,11,13]. N=30031 (Composite: 59*509).
    L = [2,3,5,7,11,13],
    N = 30031,
    Premises = [n(composite(N)), n(is_complete(L))],
    % This relies on Prime Factorization rule finding G=59, and Forward Chaining (M5-R then M4-R).
    assertion(incoherent(Premises)).

% Test The Final Theorem (Euclid's Theorem)
test(euclid_theorem_infinitude_of_primes) :-
    % The assumption that any specific finite list L is complete is incoherent.
    L = [2, 5, 11],
    % This relies on Construction, Case Analysis, and both Case 1/Case 2 logic.
    assertion(incoherent([n(is_complete(L))])).

test(euclid_theorem_empty_list) :-
    % The empty list is also incomplete (N=1+1=2, 2 is prime).
    assertion(incoherent([n(is_complete([]))])).

:- end_tests(unified_synthesis).