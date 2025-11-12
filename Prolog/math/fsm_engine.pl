/** <module> FSM Engine for Strategy Execution
 *
 * This module provides a generic Finite State Machine (FSM) execution engine
 * for running arithmetic strategies. It coordinates with the dialectical_engine
 * in the core framework while providing domain-specific extensions for
 * arithmetic operations.
 *
 * @author UMEDCA System
 * @license MIT
 */

:- module(fsm_engine, [
    run_fsm_with_base/5,
    extract_result_from_history/2
]).

:- use_module(grounded_arithmetic, [incur_cost/1]).

%! run_fsm_with_base(+Module, +InitialState, +Parameters, +Base, -History) is det.
%
% Runs an FSM strategy with base-10 tracking. This is a wrapper around
% the generic FSM runner that adds arithmetic-specific context.
%
% @param Module The module containing the FSM definition (transition/3, accept_state/1, etc.)
% @param InitialState The starting state of the FSM
% @param Parameters Additional parameters for the strategy (e.g., [A, B, Base])
% @param Base The number base being used (typically 10)
% @param History The execution history (list of steps)
%
run_fsm_with_base(Module, InitialState, Parameters, Base, History) :-
    incur_cost(fsm_initialization),
    format('  [FSM Engine] Running ~w with base ~w~n', [Module, Base]),
    run_fsm_loop(Module, InitialState, Parameters, [], History).

%! run_fsm_loop(+Module, +CurrentState, +Parameters, +Acc, -History) is det.
%
% Main FSM execution loop. Repeatedly applies transitions until
% reaching an accept state.
%
% @param Module The FSM module
% @param CurrentState Current FSM state
% @param Parameters Strategy parameters
% @param Acc History accumulator
% @param History Final execution history
%
run_fsm_loop(Module, CurrentState, _Parameters, Acc, History) :-
    % Check if we're in an accept state
    Module:accept_state(CurrentState),
    !,
    incur_cost(fsm_completion),
    reverse(Acc, History),
    format('  [FSM Engine] Reached accept state~n', []).

run_fsm_loop(Module, CurrentState, Parameters, Acc, History) :-
    % Apply a transition
    incur_cost(fsm_transition),
    Module:transition(CurrentState, NextState, Interpretation),

    % Record this step
    Step = step(CurrentState, NextState, Interpretation),

    % Continue execution
    run_fsm_loop(Module, NextState, Parameters, [Step|Acc], History).

%! extract_result_from_history(+History, -Result) is det.
%
% Extracts the final result from an FSM execution history.
% Looks for the final state and extracts the result value.
%
% @param History Execution history from run_fsm_with_base/5
% @param Result The computed result
%
extract_result_from_history(History, Result) :-
    % Get the last step
    last(History, LastStep),

    % Extract result from final state
    ( LastStep = step(_PrevState, FinalState, _Interpretation) ->
        extract_result_from_state(FinalState, Result)
    ;
        % Fallback: try to extract from interpretation
        LastStep = step(_State, _NextState, Interpretation),
        extract_result_from_interpretation(Interpretation, Result)
    ).

%! extract_result_from_state(+State, -Result) is det.
%
% Extracts the result value from a state structure.
% Handles common state representations:
% - state(Name, Result, ...)
% - state with explicit result field
%
extract_result_from_state(state(_Name, Result, _Rest), Result) :- !.
extract_result_from_state(state(_Name, Result), Result) :- !.
extract_result_from_state(State, State).  % Fallback: state IS the result

%! extract_result_from_interpretation(+Interpretation, -Result) is semidet.
%
% Attempts to extract result from interpretation string.
% This is a fallback when state structure doesn't contain result directly.
%
extract_result_from_interpretation(Interpretation, Result) :-
    % Look for "Result: N" pattern in interpretation
    atom(Interpretation),
    atomic_list_concat(Parts, 'Result: ', Interpretation),
    Parts = [_, ResultAtom|_],
    atom_number(ResultAtom, Result),
    !.
extract_result_from_interpretation(_, unknown).
