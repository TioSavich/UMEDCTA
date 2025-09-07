%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The More Machine Learner                                                %
% Author: Gemini (based on user's theoretical framework)                  %
% Date: 2025-09-07                                                        %
%                                                                         %
% Description:                                                            %
% This script models a computational learning system based on the         %
% principles of the "More Machine." It begins with only a primitive       %
% counting ability and a set of semantic rules for coherence. By          %
% observing a trace of a more sophisticated arithmetic strategy, it       %
% reflects on its own limitations and bootstraps the new strategy,        %
% dynamically generating and asserting a new automaton into its own      %
% knowledge base.                                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(more_machine_learner,
          [ bootstrap_from_observation/1 % Takes a problem, e.g., +(8,5)
          , learned_strategy/1
          ]).

% --- Foundational Abilities ---

% The system's only executable skill at the start.
:- use_module(counting_on_back, [run_counter/3]).

% The system's "teacher" or ground truth for basic facts.
:- use_module(incompatibility_semantics, [proves/1]).

% --- Dynamic Predicates for Learning ---

% This will hold the newly generated clauses for the learned strategy.
:- dynamic learned_strategy/1.

% --- The Main Bootstrapping Predicate ---

bootstrap_from_observation(+(A,B)) :-
    % 1. THE DISSONANCE: Observe a more elegant strategy for the task.
    writeln('System observes a more elegant strategy for the task...'),
    observed_rmb_trace(A, B, Trace),
    pretty_print_trace(Trace),

    % 2. THE REFLECTIVE TURN: Try to make sense of the trace.
    writeln('\nEntering reflective turn to justify the observed trace...'),
    analyze_trace(Trace, InferredRules),

    % 3. THE DIAGONAL LEAP: Construct the new automaton from inferred rules.
    writeln('\nConstructing new automaton from inferred rules...'),
    construct_automaton(+(A,B), InferredRules, AutomatonClauses),
    writeln('New automaton constructed:'),
    forall(member(Clause, AutomatonClauses), (
        write('  '), write_term(Clause, [numbervars(true)]), writeln('.')
    )),

    % 4. UP-CYCLING: Assert the new knowledge.
    retractall(learned_strategy(_)), % Clear any previously learned strategy
    assertz(learned_strategy(AutomatonClauses)),
    writeln('\nBootstrapping complete. New strategy has been asserted into the knowledge base.').


% --- The Anaphoric Trace (The "Observation") ---

% This is the external data given to the system. It's a trace of
% the RMB strategy for a given problem, e.g., 8+5.
observed_rmb_trace(A, B, [
    step(q_start, A:A, B:B),
    step(q_calc_K, K:K),
    step(q_decompose_B, B_new:B_new),
    step(q_recombine, A_new:A_new),
    step(q_accept, result:Result)
]) :-
    Base = 10,
    K is Base - A,
    B >= K, % Precondition for this trace to be valid
    B_new is B - K,
    A_new is A + K,
    Result is A_new + B_new.

% --- The Reorganizer (The Core Learning Logic) ---

% Analyzes the trace and infers the rules needed to reproduce it.
analyze_trace(Trace, [RuleK, RuleDecomp, RuleRecomb]) :-
    % Infer the rule for q_calc_K
    member(step(q_start, A:A, B:_), Trace),
    member(step(q_calc_K, K:K), Trace),
    infer_k_rule(A, K, RuleK),
    write('  - Justified [q_calc_K]: '), writeln(RuleK),

    % Infer the rule for q_decompose_B (The Critical Gap)
    member(step(q_start, A:_, B:B), Trace),
    member(step(q_decompose_B, B_new:B_new), Trace),
    infer_decomp_rule(B, K, B_new, RuleDecomp),
    write('  - Bridged Critical Gap [q_decompose_B]: '), writeln(RuleDecomp),

    % Infer the rule for q_recombine
    member(step(q_recombine, A_new:A_new), Trace),
    infer_recomb_rule(A, K, A_new, RuleRecomb),
    write('  - Justified [q_recombine]: '), writeln(RuleRecomb).


% --- Justification Sub-predicates ---

% How to find K? Use counting to find the difference to the base.
infer_k_rule(A, K, rule(calc_K, A, K)) :-
    Base = 10,
    % Use the primitive ability: count from A to Base.
    length(Ticks, K),
    maplist(=(tick), Ticks),
    run_counter(A, Ticks, Base). % If counting K ticks from A gets to Base, the rule is confirmed.

% How to decompose B? Use reflection and the coherence monitor.
infer_decomp_rule(B, K, B_new, rule(decomp_B, B, K, B_new)) :-
    % Hypothesis: decomp(B, K, B_new) is the goal.
    % The system does not know subtraction. It must test a related, simpler fact.
    % It rearranges the hypothesis into an addition problem.
    AdditionHypothesis =.. [+, K, B_new],
    FullSequent = ([AdditionHypothesis] => [B]), % Is "K + B_new" equivalent to "B"?

    % DEBUG: Print the hypothesis being tested.
    format('~N--- DEBUG: Testing sequent: ~w ---~n', [FullSequent]),

    % Ask the "teacher": Is this coherent?
    proves(FullSequent).

% How to recombine A? A simple addition, verifiable by counting.
infer_recomb_rule(A, K, A_new, rule(recomb_A, A, K, A_new)) :-
    length(Ticks, K),
    maplist(=(tick), Ticks),
    run_counter(A, Ticks, A_new). % Can verify by counting K up from A.


% --- Code Generation (The Diagonal Leap) ---

% Constructs the clauses of a new automaton predicate from the inferred rules.
% The structure of this automaton is qualitatively different from simple counting.
construct_automaton(+(A,B), Rules,
    [ (run_rmb(A_in, B_in, Result, History) :- Body)
    , (transition(q_start, A, B, state(q_calc_K, A, B, 0)))
    , (transition(q_calc_K, A, B, K, state(q_decompose_B, A, B, K)))
    , (transition(q_decompose_B, A, B, K, B_new, state(q_recombine, A, B_new, K)))
    , (transition(q_recombine, A, B_new, K, A_new, state(q_accept, A_new, B_new)))
    , (transition(q_accept, _, _, Result))
    ]) :-
    % Extract rules
    member(rule(calc_K, _, _), Rules),
    member(rule(decomp_B, _, _, B_new_val), Rules),
    member(rule(recomb_A, _, _, A_new_val), Rules),
    FinalResult is A_new_val + B_new_val,

    % Create the body of the main predicate
    Body = (
        state_machine(state(q_start, A_in, B_in), Result, History),
        % Ground truth check
        (Result =:= FinalResult -> true ; (writeln('Error: Learned strategy produced wrong result.'), fail))
    ).

% --- Helpers ---
pretty_print_trace(Trace) :-
    writeln('Observed Trace:'),
    forall(member(Step, Trace), (
        write('  '), write_term(Step, [numbervars(true)]), writeln('')
    )).

% A simple state machine driver for the generated automaton (for demonstration)
% In a full implementation, this would be the generated code itself.
state_machine(state(q_start, A, B), Result, [step(start, A, B)|History]) :-
    transition(q_start, A, B, NextState),
    state_machine(NextState, Result, History).

state_machine(state(q_calc_K, A, B, _), Result, [step(calc_K, K)|History]) :-
    Base = 10, K is Base - A,
    transition(q_calc_K, A, B, K, NextState),
    state_machine(NextState, Result, History).

state_machine(state(q_decompose_B, A, B, K), Result, [step(decomp, B_new)|History]) :-
    B_new is B - K,
    transition(q_decompose_B, A, B, K, B_new, NextState),
    state_machine(NextState, Result, History).

state_machine(state(q_recombine, A, B_new, K), Result, [step(recomb, A_new)|History]) :-
    A_new is A + K,
    transition(q_recombine, A, B_new, K, A_new, NextState),
    state_machine(NextState, Result, History).

state_machine(state(q_accept, A_final, B_final), Result, [step(accept, Result)]) :-
    Result is A_final + B_final.