/** <module> Tests for Lakoff Metaphors and Brandomian Strategies
 *
 *  Demonstrates how the PML Core Framework can critique mathematical content
 *  derived from Lakoff's embodied metaphors and Brandom's meaning-use analysis.
 *
 *  Tests include:
 *  1. Basic metaphor inferences (grounding metaphors)
 *  2. BMI pathologies (Bad Infinites)
 *  3. Strategy deployment (with and without prerequisites)
 *  4. LX-relations (elaboration hierarchies)
 *  5. Integration with ORR cycle
 */

:- ['load_math.pl'].

% =================================================================
% Test Infrastructure
% =================================================================

:- dynamic test_result/3.

run_test(Name, Goal) :-
    format('~n[TEST] ~w~n', [Name]),
    ( catch(Goal, Error, (format('  ERROR: ~w~n', [Error]), fail)) ->
        assertz(test_result(Name, pass, ok)),
        writeln('  PASS')
    ;
        assertz(test_result(Name, fail, goal_failed)),
        writeln('  FAIL')
    ).

print_summary :-
    format('~n~n=== TEST SUMMARY ===~n', []),
    findall(_, test_result(_, pass, _), Passes),
    findall(_, test_result(_, fail, _), Fails),
    length(Passes, PassCount),
    length(Fails, FailCount),
    format('Passed: ~w~n', [PassCount]),
    format('Failed: ~w~n', [FailCount]),
    (FailCount > 0 ->
        writeln('\nFailed tests:'),
        forall(test_result(Name, fail, _), format('  - ~w~n', [Name]))
    ; true).

% =================================================================
% Test Suite 1: Grounding Metaphors (The 4Gs)
% =================================================================

test_grounding_metaphors :-
    writeln('\n--- GROUNDING METAPHORS (4Gs) ---'),

    run_test('Object Collection -> Number', (
        incompatibility_semantics:proves(
            [s(collection([a,b,c])), s(size([a,b,c], 3))] => [s(number(3))],
            50, _, _
        )
    )),

    run_test('Motion Along Path -> Addition', (
        incompatibility_semantics:proves(
            [s(move_from(5, 3))] => [s(addition(5, 3))],
            50, _, _
        )
    )),

    run_test('Physical Segment -> Number', (
        incompatibility_semantics:proves(
            [s(physical_segment(7))] => [s(number(7))],
            50, _, _
        )
    )).

% =================================================================
% Test Suite 2: The Basic Metaphor of Infinity (BMI)
% =================================================================

test_bmi_metaphors :-
    writeln('\n--- BASIC METAPHOR OF INFINITY (BMI) ---'),

    run_test('BMI: Indefinite Process -> Actual Infinity', (
        incompatibility_semantics:proves(
            [s(iterative_process(counting)), s(indefinite(counting))] => [s(comp_nec(actual_infinity(counting)))],
            50, _, _
        )
    )),

    run_test('BMI: Natural Numbers as Infinite Set', (
        incompatibility_semantics:proves(
            [s(generate_naturals), s(indefinite(generate_naturals))] => [s(comp_nec(infinite_set(naturals)))],
            50, _, _
        )
    )),

    run_test('BMI: Sequence Approaching Limit', (
        incompatibility_semantics:proves(
            [s(sequence([1, 1/2, 1/3, 1/4])), s(approaches([1, 1/2, 1/3, 1/4], 0))] => [s(comp_nec(limit([1, 1/2, 1/3, 1/4], 0)))],
            50, _, _
        )
    )).

% =================================================================
% Test Suite 3: BMI Pathologies (Bad Infinites)
% =================================================================

test_bmi_pathologies :-
    writeln('\n--- BMI PATHOLOGIES (Bad Infinites) ---'),

    run_test('Pathology: Being <-> Nothing Cycle', (
        incompatibility_semantics:proves(
            [s(empty_set)] => [s(comp_nec(being))],
            50, _, Proof1
        ),
        incompatibility_semantics:proves(
            [s(being)] => [s(comp_nec(nothing))],
            50, _, Proof2
        ),
        writeln('  Detected oscillation: empty_set -> Being -> Nothing'),
        Proof1 \= erasure(_),
        Proof2 \= erasure(_)
    )),

    run_test('Pathology: Zeno\'s Paradox (Incoherence)', (
        % Zeno's paradox: Both material inferences are valid, creating contradiction
        incompatibility_semantics:proves(
            [s(motion(achilles)), s(infinite_subdivisions(achilles))] => [s(comp_nec(completes(achilles)))],
            50, _, _
        ),
        incompatibility_semantics:proves(
            [s(motion(achilles)), s(infinite_subdivisions(achilles))] => [s(comp_nec(neg(completes(achilles))))],
            50, _, _
        ),
        writeln('  Detected Zeno contradiction: both completes and neg(completes) provable')
    )),

    run_test('Pathology: Russell\'s Paradox Detection', (
        incompatibility_semantics:incoherent([
            s(comp_nec(set_of_all_sets))
        ])
    )).

% =================================================================
% Test Suite 4: Arithmetic Strategies
% =================================================================

test_arithmetic_strategies :-
    writeln('\n--- ARITHMETIC STRATEGIES ---'),

    run_test('Sliding: Difference Invariance', (
        % Test that the material inference exists (not that equals is provable)
        incompatibility_semantics:material_inference(
            [s(subtraction(A, B)), s(shift(C))],
            s(equals(subtraction(A, B), subtraction(add(A, C), add(B, C)))),
            _Body
        )
    )),

    run_test('Counting On: Addition as Sequence', (
        incompatibility_semantics:proves(
            [s(addition(5, 3))] => [s(count_steps(3, starting_from(5)))],
            50, _, _
        )
    )),

    run_test('Rearranging to Make Bases: Strategic Decomposition', (
        incompatibility_semantics:proves(
            [s(addition(28, 7)), s(decompose(7, 2, 5)), s(next_base(28, 2))] => [s(equals(addition(28, 7), addition(addition(28, 2), 5)))],
            50, _, _
        )
    )),

    run_test('RMB: Creates Simplified Problem', (
        incompatibility_semantics:proves(
            [s(problem(addition(28, 7)))] => [s(comp_nec(decompose(7, 2, 5)))],
            50, _, _
        )
    )).

% =================================================================
% Test Suite 5: PP-Necessities and Pathologies
% =================================================================

test_prerequisites :-
    writeln('\n--- PP-NECESSITIES AND PATHOLOGIES ---'),

    run_test('Prerequisite Violation: Sliding without Number Line', (
        incompatibility_semantics:incoherent([
            s(deploys(alice, sliding)),
            s(neg(possesses(alice, number_line_intuition)))
        ])
    )),

    run_test('Sufficiency Condition: Can Deploy Counting On', (
        incompatibility_semantics:proves(
            [s(possesses(bob, iterated_succession)), s(possesses(bob, termination_condition))] => [s(exp_poss(deploys(bob, counting_on)))],
            50, _, _
        )
    )),

    run_test('Strategy Declaration: Sliding is a Strategy', (
        arithmetic_strategies:strategy(sliding)
    )),

    run_test('PP-Necessity Query: Sliding requires Number Line', (
        arithmetic_strategies:pp_necessity(sliding, number_line_intuition)
    )).

% =================================================================
% Test Suite 6: LX-Relations (Elaboration)
% =================================================================

test_lx_relations :-
    writeln('\n--- LX-RELATIONS (Elaboration Hierarchies) ---'),

    run_test('LX Declaration: RMB elaborates Counting On', (
        arithmetic_strategies:elaborates(rearranging_to_make_bases, counting_on)
    )),

    run_test('LX Inference: Elaboration is Compressive', (
        incompatibility_semantics:proves(
            [s(elaborates(rearranging_to_make_bases, counting_on))] => [s(comp_nec(lx_relation(rearranging_to_make_bases, counting_on)))],
            50, _, _
        )
    )),

    run_test('LX Consequence: Provides Metavocabulary', (
        incompatibility_semantics:proves(
            [s(lx_relation(rmb, counting_on)), s(deploys(alice, rmb))] => [s(exp_nec(can_articulate_principles_of(alice, counting_on)))],
            50, _, _
        )
    )).

% =================================================================
% Test Suite 7: Integration with PML Dynamics
% =================================================================

test_pml_integration :-
    writeln('\n--- INTEGRATION WITH PML DYNAMICS ---'),

    run_test('Strategy is Compressive', (
        incompatibility_semantics:proves(
            [s(enact(alice, sliding, problem(18-9)))] => [s(comp_nec(simplified(problem(18-9))))],
            50, _, _
        )
    )),

    run_test('Compression Leads to Expansion', (
        incompatibility_semantics:proves(
            [s(simplified(problem(p))), s(solve(problem(p), answer(42)))] => [s(exp_nec(answer(answer(42))))],
            50, _, _
        )
    )),

    run_test('Failure Creates Tension', (
        incompatibility_semantics:proves(
            [s(enact(alice, sliding, problem(p))), s(failure(sliding, missing_number_line))] => [s(comp_nec(awareness_of_inadequacy(alice, missing_number_line)))],
            50, _, _
        )
    )),

    run_test('Tension Enables Reflection', (
        incompatibility_semantics:proves(
            [s(awareness_of_inadequacy(alice, reason))] => [s(exp_poss(triggers_critique(reason)))],
            50, _, _
        )
    )).

% =================================================================
% Test Suite 8: ORR Cycle with Strategy Failure
% =================================================================

test_orr_cycle :-
    writeln('\n--- ORR CYCLE WITH STRATEGY FAILURE ---'),

    run_test('Observe: Strategy Deployed', (
        incompatibility_semantics:proves(
            [s(problem(addition(28, 7)))] => [s(comp_nec(decompose(7, 2, 5)))],
            50, _, Proof
        ),
        Proof \= erasure(_)
    )),

    run_test('Reflect: Failure Creates Perturbation', (
        incompatibility_semantics:proves(
            [s(enact(alice, sliding, problem(p))), s(failure(sliding, missing_prerequisite(number_line_intuition)))] => [s(comp_nec(awareness_of_inadequacy(alice, missing_prerequisite(number_line_intuition))))],
            50, _, _
        )
    )),

    run_test('Reorganize: Accommodation Signal', (
        % This doesn't actually accommodate (intentional), but signals the need
        \+ critique:accommodate(perturbation(strategy_failure, missing_prerequisite(number_line_intuition)))
    )).

% =================================================================
% Test Suite 9: Conceptual Blends (Euler)
% =================================================================

test_conceptual_blends :-
    writeln('\n--- CONCEPTUAL BLENDS ---'),

    run_test('Euler\'s Blend: e^(iπ) + 1 = 0', (
        incompatibility_semantics:proves(
            [s(exp_function(i_pi)), s(unit_circle), s(infinite_series_expansion)] => [s(comp_nec(equals(add(exp(i_pi), 1), 0)))],
            50, _, _
        )
    )),

    run_test('Functions Are Numbers Metaphor', (
        incompatibility_semantics:proves(
            [s(function(sin, x))] => [s(number(result(sin, x)))],
            50, _, _
        )
    )).

% =================================================================
% Run All Tests
% =================================================================

run_all_tests :-
    retractall(test_result(_, _, _)),
    writeln('=== LAKOFF & BRANDOM INTEGRATION TEST SUITE ==='),

    test_grounding_metaphors,
    test_bmi_metaphors,
    test_bmi_pathologies,
    test_arithmetic_strategies,
    test_prerequisites,
    test_lx_relations,
    test_pml_integration,
    test_orr_cycle,
    test_conceptual_blends,

    print_summary.

:- initialization(run_all_tests, main).
