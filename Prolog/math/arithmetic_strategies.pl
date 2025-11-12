/** <module> Brandomian Analysis of Arithmetic Strategies
 *
 *  Represents arithmetic strategies (Sliding, Counting On, Rearranging to Make Bases)
 *  as practices with PP-necessities and PP-sufficiencies, following Brandom's
 *  meaning-use analysis.
 *
 *  Each strategy is modeled as:
 *  1. Material inferences (the "how" of the strategy)
 *  2. PP-necessities (prerequisite practices)
 *  3. PP-sufficiencies (practices sufficient to deploy)
 *  4. Circumstances and Consequences of Application
 *
 *  These can be subjected to critique to identify:
 *  - Missing prerequisites
 *  - Inefficient deployments
 *  - LX-relations (elaboration hierarchies)
 */

:- module(arithmetic_strategies, [
    strategy/1,
    pp_necessity/2,
    pp_sufficiency/2,
    elaborates/2
]).

% Declare discontiguous predicates (definitions spread across file)
:- discontiguous pp_necessity/2.
:- discontiguous pp_sufficiency/2.

% Ensure operators are available
:- op(500, fx, comp_nec).
:- op(500, fx, exp_nec).
:- op(500, fx, exp_poss).
:- op(500, fx, comp_poss).
:- op(500, fx, neg).
:- op(1050, xfy, =>).

% =================================================================
% Strategy Declarations
% =================================================================

strategy(sliding).
strategy(counting_on).
strategy(rearranging_to_make_bases).

% =================================================================
% Strategy 1: "Sliding" (Additive Invariance)
% =================================================================

% Central Material Inference: (a - b) = (a + c) - (b + c)
% The difference between two numbers is invariant under parallel shifts

incompatibility_semantics:material_inference(
    [s(subtraction(A, B)), s(shift(C))],
    s(equals(subtraction(A, B), subtraction(add(A, C), add(B, C)))),
    true  % Strategy: sliding
).

% Example: 18 - 9 = 19 - 10 = 9

incompatibility_semantics:material_inference(
    [s(problem(subtraction(18, 9)))],
    s(comp_nec(shift_to(subtraction(19, 10)))),
    true  % Sliding makes problem easier
).

% PP-Necessities for Sliding

pp_necessity(sliding, number_line_intuition).
pp_necessity(sliding, basic_arithmetic).
pp_necessity(sliding, base_10_structure).

incompatibility_semantics:material_inference(
    [s(practice(sliding)), s(neg(number_line_intuition))],
    s(comp_nec(failure(sliding))),
    missing_prerequisite
).

% PP-Sufficiencies for Sliding

pp_sufficiency(sliding, difference_invariance).
pp_sufficiency(sliding, strategic_adjustment).

incompatibility_semantics:material_inference(
    [s(possesses(Agent, difference_invariance)),
     s(possesses(Agent, strategic_adjustment))],
    s(exp_poss(deploys(Agent, sliding))),
    true  % Sufficiency condition for deployment
).

% =================================================================
% Strategy 2: "Counting On"
% =================================================================

% Central Material Inference: a + b = result_of_counting_b_steps_from_a
% Addition is enacted as rhythmic succession through the number sequence

incompatibility_semantics:material_inference(
    [s(addition(A, B))],
    s(count_steps(B, starting_from(A))),
    true  % Strategy: counting on
).

% Example: 5 + 3 enacted as "6, 7, 8"

incompatibility_semantics:material_inference(
    [s(problem(addition(5, 3)))],
    s(comp_nec(sequence([6, 7, 8]))),
    true  % Counting on generates sequence
).

% PP-Necessities for Counting On

pp_necessity(counting_on, stable_order_principle).
pp_necessity(counting_on, one_to_one_correspondence).
pp_necessity(counting_on, cardinality_principle).
pp_necessity(counting_on, number_recognition).

% PP-Sufficiencies for Counting On

pp_sufficiency(counting_on, iterated_succession).
pp_sufficiency(counting_on, termination_condition).

incompatibility_semantics:material_inference(
    [s(possesses(Agent, iterated_succession)),
     s(possesses(Agent, termination_condition))],
    s(exp_poss(deploys(Agent, counting_on))),
    true  % Sufficiency condition for deployment
).

% =================================================================
% Strategy 3: "Rearranging to Make Bases" (RMB)
% =================================================================

% Central Material Inference: A + B = A + (K + R) = (A + K) + R
% Strategic decomposition to create multiples of 10

incompatibility_semantics:material_inference(
    [s(addition(A, B)), s(decompose(B, K, R)), s(next_base(A, K))],
    s(equals(addition(A, B), addition(addition(A, K), R))),
    true  % Strategy: rearranging to make bases
).

% Example: 28 + 7 = 28 + (2 + 5) = (28 + 2) + 5 = 30 + 5 = 35

incompatibility_semantics:material_inference(
    [s(problem(addition(28, 7)))],
    s(comp_nec(decompose(7, 2, 5))),
    true  % RMB strategic decomposition
).

incompatibility_semantics:material_inference(
    [s(decompose(7, 2, 5)), s(addition(28, 2))],
    s(comp_nec(simplified_problem(addition(30, 5)))),
    true  % RMB creates base
).

% PP-Necessities for RMB

pp_necessity(rearranging_to_make_bases, counting_on).
pp_necessity(rearranging_to_make_bases, base_10_structure).
pp_necessity(rearranging_to_make_bases, number_decomposition).

% PP-Sufficiencies for RMB

pp_sufficiency(rearranging_to_make_bases, gap_calculation).
pp_sufficiency(rearranging_to_make_bases, strategic_decomposition).
pp_sufficiency(rearranging_to_make_bases, reassociation).

incompatibility_semantics:material_inference(
    [s(possesses(Agent, gap_calculation)),
     s(possesses(Agent, strategic_decomposition)),
     s(possesses(Agent, reassociation))],
    s(exp_poss(deploys(Agent, rearranging_to_make_bases))),
    true  % Sufficiency condition for deployment
).

% =================================================================
% LX-Relations: Elaboration Hierarchies
% =================================================================

% "Rearranging to Make Bases" is LX for "Counting On"
% RMB makes explicit the principles (associativity, decomposition) that are
% implicit in the simpler Counting On strategy

elaborates(rearranging_to_make_bases, counting_on).

incompatibility_semantics:material_inference(
    [s(elaborates(Strategy_Explicit, Strategy_Implicit))],
    s(comp_nec(lx_relation(Strategy_Explicit, Strategy_Implicit))),
    true  % Brandomian elaboration
).

% The LX-relation means: Strategy_Explicit allows you to SAY what you could only DO with Strategy_Implicit

incompatibility_semantics:material_inference(
    [s(lx_relation(S_Explicit, S_Implicit)), s(deploys(Agent, S_Explicit))],
    s(exp_nec(can_articulate_principles_of(Agent, S_Implicit))),
    true  % LX provides metavocabulary
).

% =================================================================
% Pathologies in Arithmetic Strategies
% =================================================================

% Pathology 1: Deploying a strategy without prerequisites
% This creates a failure mode

incompatibility_semantics:material_inference(
    [s(deploys(Agent, Strategy)), s(pp_necessity(Strategy, Prerequisite)), s(neg(possesses(Agent, Prerequisite)))],
    s(comp_nec(failure(Strategy, missing_prerequisite(Prerequisite)))),
    true  % Prerequisite violation
).

% Example: Trying to use Sliding without number line intuition

incompatibility_semantics:is_incoherent(X) :-
    member(s(deploys(Agent, sliding)), X),
    member(s(neg(possesses(Agent, number_line_intuition))), X),
    writeln('  PATHOLOGY: Cannot deploy Sliding without Number Line Intuition').

% Pathology 2: Circular dependency in prerequisites
% If Strategy A requires B, and B requires A, we have a Bad Infinite

incompatibility_semantics:is_incoherent(X) :-
    member(s(pp_necessity(Strategy_A, Strategy_B)), X),
    member(s(pp_necessity(Strategy_B, Strategy_A)), X),
    writeln('  PATHOLOGY: Circular prerequisite dependency detected').

% =================================================================
% Integration with PML Dynamics
% =================================================================

% Strategies are enacted in the Subjective modal context (S)
% They compress problems into simpler forms (compressive necessity)

incompatibility_semantics:material_inference(
    [s(enact(Agent, Strategy, Problem))],
    s(comp_nec(simplified(Problem))),
    true  % Strategy is compressive
).

% Successful strategy deployment leads to expansive release (the answer)

incompatibility_semantics:material_inference(
    [s(simplified(Problem)), s(solve(Problem, Answer))],
    s(exp_nec(answer(Answer))),
    true  % Compression leads to expansion
).

% Failed strategy deployment creates tension (awareness of inadequacy)

incompatibility_semantics:material_inference(
    [s(enact(Agent, Strategy, Problem)), s(failure(Strategy, Reason))],
    s(comp_nec(awareness_of_inadequacy(Agent, Reason))),
    true  % Failure creates tension
).

% This tension can trigger the ORR cycle (critique and accommodation)

incompatibility_semantics:material_inference(
    [s(awareness_of_inadequacy(Agent, Reason))],
    s(exp_poss(triggers_critique(Reason))),
    true  % Tension enables reflection
).

% =================================================================
% Meaning-Use Diagrams (MUDs) as Proof Structures
% =================================================================

% A MUD is a graph showing relationships between vocabulary (V-space) and practices (P-space)
% In PML, this is represented as a proof tree where:
% - V-space nodes are vocabulary terms in the antecedent/consequent
% - P-space nodes are practices in the justification conditions
% - Edges are material inferences

% MUD for Counting On:
% V1: Problem "n + m" -> P5: Iterated Succession
% P5: Generates sequence -> V2: Counting Sequence
% V2: Stops after m steps -> P6: Termination Condition
% P6: Identifies last number -> V3: Final Utterance
% V3: Is the answer -> V4: Answer

mud(counting_on, [
    edge(problem(addition(N, M)), iterated_succession),
    edge(iterated_succession, counting_sequence(N, M)),
    edge(counting_sequence(N, M), termination_condition(M)),
    edge(termination_condition(M), final_utterance),
    edge(final_utterance, answer)
]).

% A valid MUD corresponds to a valid proof in incompatibility_semantics

incompatibility_semantics:material_inference(
    [s(mud(Strategy, Edges)), s(all_edges_valid(Edges))],
    s(comp_nec(valid_meaning_use_analysis(Strategy))),
    true  % MUD validity
).

% =================================================================
% Commentary
% =================================================================

% This module demonstrates how Brandomian meaning-use analysis can be
% represented in the PML framework:
%
% 1. Strategies are PRACTICES with material-inferential content
%    They are not just procedures, but ways of making sense of problems
%
% 2. PP-necessities and PP-sufficiencies are NORMATIVE STATUSES
%    They determine what is CORRECT (not just what is possible)
%
% 3. LX-relations create ELABORATION HIERARCHIES
%    More sophisticated strategies make explicit what simpler ones leave implicit
%
% 4. Pathologies arise when:
%    - Prerequisites are missing (failure to deploy)
%    - Circular dependencies exist (Bad Infinite)
%    - Strategies are applied outside their domain of applicability
%
% 5. Strategy deployment is MODAL:
%    - Enacting a strategy is COMPRESSIVE (simplification)
%    - Getting the answer is EXPANSIVE (release)
%    - Failure creates TENSION (awareness, the "A" in U -> A -> LG -> U')
%
% 6. The ORR cycle can be triggered by strategy failure
%    - Observe: Strategy deployed
%    - Reflect: Failure detected (missing prerequisite, incoherence)
%    - Reorganize: Accommodate by learning prerequisite or switching strategy
%    - Retry: Deploy revised strategy
%
% This is how practices become CONTENT for critique.
