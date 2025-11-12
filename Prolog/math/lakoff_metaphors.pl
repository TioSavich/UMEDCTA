/** <module> Lakoff's Embodied Mathematical Metaphors
 *
 *  Represents conceptual metaphors from Lakoff & Núñez's "Where Mathematics Comes From"
 *  as material inferences within the PML Core Framework.
 *
 *  These metaphors are grounding and linking mechanisms that map sensory-motor experience
 *  onto abstract mathematical concepts. They can exhibit pathologies (e.g., Bad Infinites)
 *  and are subject to critique.
 *
 *  Organization:
 *  - Part 1: The 4Gs (Grounding Metaphors for Arithmetic)
 *  - Part 2: Linking Metaphors (Algebra, Logic, Sets)
 *  - Part 3: The Basic Metaphor of Infinity (BMI) and variants
 */

:- module(lakoff_metaphors, []).

% Ensure operators are available
:- op(500, fx, comp_nec).
:- op(500, fx, exp_nec).
:- op(500, fx, exp_poss).
:- op(500, fx, comp_poss).
:- op(500, fx, neg).
:- op(1050, xfy, =>).

% =================================================================
% Part 1: The 4Gs - Grounding Metaphors for Arithmetic
% =================================================================

% Metaphor 1: Arithmetic Is Object Collection
% Maps: Collections → Numbers, Putting together → Addition, Taking apart → Subtraction

incompatibility_semantics:material_inference(
    [s(collection(Objects)), s(size(Objects, N))],
    s(number(N)),
    true
).

incompatibility_semantics:material_inference(
    [s(put_together(C1, C2)), s(size(C1, N1)), s(size(C2, N2))],
    s(addition(N1, N2)),
    true
).

incompatibility_semantics:material_inference(
    [s(take_apart(C_Large, C_Small)), s(size(C_Large, N_Large)), s(size(C_Small, N_Small))],
    s(subtraction(N_Large, N_Small)),
    true
).

% Metaphor 2: Arithmetic Is Object Construction
% Maps: Wholes/Parts → Numbers, Fitting together → Multiplication, Splitting → Division

incompatibility_semantics:material_inference(
    [s(whole(W)), s(parts(W, Parts))],
    s(number_structure(W, Parts)),
    true
).

incompatibility_semantics:material_inference(
    [s(fit_together(Parts, Count)), s(each_part_size(S))],
    s(multiplication(S, Count)),
    true
).

% Metaphor 3: The Measuring Stick Metaphor
% Maps: Physical segments → Numbers, End-to-end placement → Addition

incompatibility_semantics:material_inference(
    [s(physical_segment(Length))],
    s(number(Length)),
    true
).

incompatibility_semantics:material_inference(
    [s(place_end_to_end(Seg1, Seg2)), s(length(Seg1, L1)), s(length(Seg2, L2))],
    s(addition(L1, L2)),
    true
).

% Metaphor 4: Arithmetic Is Motion Along a Path
% Maps: Point-locations → Numbers, Moving away from origin → Addition

incompatibility_semantics:material_inference(
    [s(point_location(Loc))],
    s(number(Loc)),
    true
).

incompatibility_semantics:material_inference(
    [s(move_from(A, Distance))],
    s(addition(A, Distance)),
    true
).

% =================================================================
% Part 2: Linking Metaphors
% =================================================================

% Numbers Are Points on a Line (The Number Line)
% This is a fundamental linking metaphor

incompatibility_semantics:material_inference(
    [s(point_on_line(X))],
    s(number(X)),
    true
).

incompatibility_semantics:material_inference(
    [s(distance(X, Y, D))],
    s(arithmetic_difference(X, Y, D)),
    true
).

% Classes Are Containers (Boole's Metaphor)
% Maps: Bounded regions → Classes, Objects inside → Members

incompatibility_semantics:material_inference(
    [s(bounded_region(R)), s(objects_inside(R, Objs))],
    s(class(R, Objs)),
    true
).

incompatibility_semantics:material_inference(
    [s(union(R1, R2))],
    s(class_union(R1, R2)),
    true
).

% =================================================================
% Part 3: The Basic Metaphor of Infinity (BMI) and Special Cases
% =================================================================

% The Basic Metaphor of Infinity (Core)
% Source: Completed iterative processes
% Target: Processes that go on indefinitely
% DANGER: This is where Bad Infinites can emerge

incompatibility_semantics:material_inference(
    [s(iterative_process(P)), s(indefinite(P))],
    s(comp_nec(actual_infinity(P))),
    true
).

% BMI Special Case: Infinity As a "Number"
% The sequence 0, 1, 2, 3, ... metaphorically "ends" with ∞

incompatibility_semantics:material_inference(
    [s(enumeration_sequence(Integers)), s(indefinite(Integers))],
    s(comp_nec(infinity_number)),
    true
).

% BMI Special Case: The Infinite Set of Natural Numbers
% The process of generating naturals metaphorically "completes"

incompatibility_semantics:material_inference(
    [s(generate_naturals), s(indefinite(generate_naturals))],
    s(comp_nec(infinite_set(naturals))),
    true
).

% BMI Special Case: Infinite Sequences and Limits
% A sequence "approaching" a limit via fictive motion

incompatibility_semantics:material_inference(
    [s(sequence(Terms)), s(approaches(Terms, Limit))],
    s(comp_nec(limit(Terms, Limit))),
    true
).

% Fictive Motion: Sequence "approaches" limit
% This uses the Motion Along a Path metaphor

incompatibility_semantics:material_inference(
    [s(sequence(Terms)), s(distance_from(Terms, Limit, Zero_At_Infinity))],
    s(approaches(Terms, Limit)),
    true
).

% BMI Special Case: Infinitesimals
% The process 1/n for increasing n metaphorically yields infinitesimal δ
% PATHOLOGY RISK: This is a compressive cycle

incompatibility_semantics:material_inference(
    [s(iterative_inverse(n)), s(indefinite(n))],
    s(comp_nec(infinitesimal(delta))),
    true
).

% =================================================================
% Pathological Cases: Where BMI Leads to Bad Infinites
% =================================================================

% Bad Infinite: Being <-> Nothing (Hegelian)
% The BMI can create oscillations between emptiness and fullness

incompatibility_semantics:material_inference(
    [s(empty_set)],
    s(comp_nec(being)),
    true  % BMI creates being from nothing
).

incompatibility_semantics:material_inference(
    [s(being)],
    s(comp_nec(nothing)),
    true  % Being collapses to emptiness
).

% Bad Infinite: 0.999... = 1
% The BMI for infinite decimals creates identity between distinct processes
% This is actually NOT pathological in standard mathematics, but can be seen as such

incompatibility_semantics:material_inference(
    [s(infinite_decimal(nines))],
    s(comp_nec(equals(infinite_decimal(nines), 1))),
    true  % BMI for infinite decimals
).

% Bad Infinite: Zeno's Paradox
% The BMI applied to motion yields contradiction
% Achilles "completes" infinite subdivisions, but motion "cannot complete"

incompatibility_semantics:material_inference(
    [s(motion(achilles)), s(infinite_subdivisions(achilles))],
    s(comp_nec(completes(achilles))),
    true  % BMI says infinite process completes
).

incompatibility_semantics:material_inference(
    [s(motion(achilles)), s(infinite_subdivisions(achilles))],
    s(comp_nec(neg(completes(achilles)))),
    true  % Physical intuition says cannot complete infinite steps
).

% =================================================================
% Conceptual Blends
% =================================================================

% Euler's Blend: e^(iπ) + 1 = 0
% This is a masterpiece of conceptual integration, blending:
% - Functions Are Numbers
% - Arithmetic Is Motion
% - Trigonometry via Unit Circle
% - The BMI for infinite series

incompatibility_semantics:material_inference(
    [s(exp_function(i_pi)), s(unit_circle), s(infinite_series_expansion)],
    s(comp_nec(equals(add(exp(i_pi), 1), 0))),
    true  % Euler's blend of multiple metaphors
).

% Functions Are Numbers Metaphor
% Allows f(x) to be treated as a number

incompatibility_semantics:material_inference(
    [s(function(F, X))],
    s(number(result(F, X))),
    true  % Functions are numbers - result is treated as numeric value
).

% =================================================================
% Meta-Level: Metaphors About Metaphors
% =================================================================

% Grounding Metaphors Are Foundational
% These create the initial mapping from embodiment to abstraction

incompatibility_semantics:material_inference(
    [s(grounding_metaphor(M)), s(sensory_motor_domain(D_Source))],
    s(comp_nec(foundational(M))),
    true
).

% Linking Metaphors Extend Mathematics
% These map between mathematical domains

incompatibility_semantics:material_inference(
    [s(linking_metaphor(M)), s(mathematical_domain(D_Source)), s(mathematical_domain(D_Target))],
    s(enables_abstraction(M)),
    true
).

% The BMI Is a Compression Operator
% It "completes" indefinite processes, creating actual infinity

incompatibility_semantics:material_inference(
    [s(bmi_applied(Process))],
    s(comp_nec(compression(Process, actual_infinity))),
    true  % BMI is a compression operator
).

% =================================================================
% Stress Points: Where Metaphors Can Break Down
% =================================================================

% The BMI can create incoherence when applied carelessly
% Example: "The set of all sets" (Russell's Paradox)

incompatibility_semantics:material_inference(
    [s(bmi_applied(set_formation)), s(unrestricted(set_formation))],
    s(comp_nec(set_of_all_sets)),
    true
).

incompatibility_semantics:is_incoherent(X) :-
    member(s(comp_nec(set_of_all_sets)), X),
    writeln('  PATHOLOGY: Russell\'s Paradox - set of all sets is incoherent').

% The BMI for limits requires "teaser elements" (epsilons)
% Without them, the compression is too fast and creates Bad Infinites

incompatibility_semantics:material_inference(
    [s(bmi_applied(sequence_limit)), s(neg(epsilon_delta_method))],
    s(comp_nec(pathological_limit)),
    true  % BMI without safeguards creates pathologies
).

% =================================================================
% Commentary
% =================================================================

% This module demonstrates how Lakoff's embodied metaphors can be represented
% as material inferences in the PML framework. Key observations:
%
% 1. Grounding Metaphors (4Gs) are inference-preserving mappings from
%    sensory-motor domains (collections, construction, measurement, motion)
%    to abstract arithmetic.
%
% 2. Linking Metaphors extend mathematics by mapping between domains
%    (e.g., numbers as points, classes as containers).
%
% 3. The Basic Metaphor of Infinity (BMI) is a COMPRESSIVE operation
%    that adds a "final resultant state" to indefinite processes.
%    This is explicitly modeled as comp_nec(actual_infinity).
%
% 4. The BMI is PATHOLOGY-PRONE. It can create:
%    - Bad Infinites (Being <-> Nothing, Zeno's Paradox)
%    - Incoherence (Russell's Paradox)
%    - Counterintuitive identities (0.999... = 1)
%
% 5. These pathologies can be DETECTED by the critique.pl module
%    via cycle detection and incoherence checking.
%
% 6. Sublation is required to resolve Bad Infinites:
%    - Being <-> Nothing requires "Becoming"
%    - Zeno's Paradox requires Calculus (limits or infinitesimals)
%    - Russell's Paradox requires Type Theory or ZFC axioms
%
% This is how conceptual metaphors become CONTENT for the PML system to critique.
