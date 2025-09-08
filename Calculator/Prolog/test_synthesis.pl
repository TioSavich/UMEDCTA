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

:- end_tests(unified_synthesis).