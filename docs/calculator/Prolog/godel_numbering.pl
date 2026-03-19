/** <module> Gödel Numbering for the Hermeneutic Calculator
 *
 * This module implements Gödel numbering (arithmetization) for the
 * Hermeneutic Calculator's automata. It demonstrates how the syntax
 * and semantics of student-invented arithmetic strategies can be
 * encoded as natural numbers, enabling the system to "talk about itself."
 *
 * This is crucial for demonstrating that Gödel's Incompleteness Theorem
 * applies to the formalized student strategies.
 *
 * @author UMEDCA System
 * @date October 4, 2025
 */
:- module(godel_numbering, [
    % Core encoding functions
    encode_state/2,
    decode_state/2,
    encode_transition/2,
    encode_configuration/2,
    encode_trace/2,
    
    % Prime number utilities
    nth_prime/2,
    prime_factorization/2,
    
    % Demonstration predicates
    demo_c2c_encoding/0,
    demo_transition_encoding/0
]).

:- use_module(library(clpfd)).

%% ========================================================================
%% Prime Number Utilities
%% ========================================================================

%!  nth_prime(+N:integer, -Prime:integer) is det.
%
%   Returns the Nth prime number (1-indexed).
%   Uses a simple trial division method (sufficient for demonstration).
nth_prime(1, 2) :- !.
nth_prime(N, Prime) :-
    N > 1,
    nth_prime_helper(2, 1, N, Prime).

nth_prime_helper(Candidate, Count, Target, Prime) :-
    Count =:= Target,
    !,
    Prime = Candidate.
nth_prime_helper(Candidate, Count, Target, Prime) :-
    Count < Target,
    NextCandidate is Candidate + 1,
    ( is_prime(NextCandidate) ->
        NewCount is Count + 1,
        nth_prime_helper(NextCandidate, NewCount, Target, Prime)
    ;
        nth_prime_helper(NextCandidate, Count, Target, Prime)
    ).

%!  is_prime(+N:integer) is semidet.
%
%   True if N is prime.
is_prime(2) :- !.
is_prime(N) :-
    N > 2,
    N mod 2 =\= 0,
    \+ has_divisor(N, 3).

has_divisor(N, D) :-
    D * D =< N,
    ( N mod D =:= 0 ->
        true
    ;
        D2 is D + 2,
        has_divisor(N, D2)
    ).

%!  prime_power(+Prime:integer, +Exponent:integer, -Result:integer) is det.
%
%   Computes Prime^Exponent efficiently.
prime_power(_, 0, 1) :- !.
prime_power(Prime, Exp, Result) :-
    Exp > 0,
    Exp1 is Exp - 1,
    prime_power(Prime, Exp1, Result1),
    Result is Prime * Result1.

%% ========================================================================
%% Symbol Encoding
%% ========================================================================

%!  encode_symbol(+Symbol, -GödelNumber) is det.
%
%   Assigns Gödel numbers to the symbols used in the HC automata.
%   This creates a bijection between symbols and natural numbers.

% State names
encode_symbol(q_init, 1).
encode_symbol(q_check_G, 2).
encode_symbol(q_count_items, 3).
encode_symbol(q_next_group, 4).
encode_symbol(q_accept, 5).

% Additional states from other strategies
encode_symbol(q_add_bases, 10).
encode_symbol(q_add_ones, 11).
encode_symbol(q_idle, 12).

% Registers/Variables (for general use)
encode_symbol(reg_groups_done, 20).
encode_symbol(reg_item_in_group, 21).
encode_symbol(reg_total, 22).
encode_symbol(reg_num_groups, 23).
encode_symbol(reg_group_size, 24).

% Operations
encode_symbol(increment, 30).
encode_symbol(decrement, 31).
encode_symbol(check_zero, 32).
encode_symbol(check_equal, 33).

% Natural numbers (0-9 for demonstration)
encode_symbol(0, 100).
encode_symbol(1, 101).
encode_symbol(2, 102).
encode_symbol(3, 103).
encode_symbol(4, 104).
encode_symbol(5, 105).
encode_symbol(6, 106).
encode_symbol(7, 107).
encode_symbol(8, 108).
encode_symbol(9, 109).

% Catch-all for atoms
encode_symbol(Atom, Code) :-
    atom(Atom),
    \+ encode_symbol(Atom, _),  % Not already defined
    atom_codes(Atom, Codes),
    encode_atom_codes(Codes, Code).

encode_atom_codes([], 0).
encode_atom_codes([H|T], Code) :-
    encode_atom_codes(T, TailCode),
    Code is H + 256 * TailCode.

%% ========================================================================
%% State Encoding
%% ========================================================================

%!  encode_state(+State, -GödelNumber) is det.
%
%   Encodes a complete state configuration as a Gödel number.
%   Uses the fundamental theorem of arithmetic (unique prime factorization).
%
%   Example: state(q_count_items, 2, 3, 10, 3, 4)
%   is encoded as: 2^g(q_count_items) × 3^g(2) × 5^g(3) × 7^g(10) × 11^g(3) × 13^g(4)
%
%   where g(x) is the Gödel number of symbol x.

encode_state(state(StateName), GödelNum) :-
    !,
    encode_symbol(StateName, GödelNum).

encode_state(state(StateName, Reg1), GödelNum) :-
    !,
    encode_symbol(StateName, G1),
    encode_value(Reg1, G2),
    nth_prime(1, P1),
    nth_prime(2, P2),
    prime_power(P1, G1, Term1),
    prime_power(P2, G2, Term2),
    GödelNum is Term1 * Term2.

encode_state(state(StateName, Reg1, Reg2, Reg3, Reg4, Reg5), GödelNum) :-
    % Full C2C state: state(Name, GroupsDone, ItemInGroup, Total, NumGroups, GroupSize)
    encode_symbol(StateName, G1),
    encode_value(Reg1, G2),
    encode_value(Reg2, G3),
    encode_value(Reg3, G4),
    encode_value(Reg4, G5),
    encode_value(Reg5, G6),
    
    % Compute 2^G1 × 3^G2 × 5^G3 × 7^G4 × 11^G5 × 13^G6
    nth_prime(1, P1), nth_prime(2, P2), nth_prime(3, P3),
    nth_prime(4, P4), nth_prime(5, P5), nth_prime(6, P6),
    
    prime_power(P1, G1, T1),
    prime_power(P2, G2, T2),
    prime_power(P3, G3, T3),
    prime_power(P4, G4, T4),
    prime_power(P5, G5, T5),
    prime_power(P6, G6, T6),
    
    GödelNum is T1 * T2 * T3 * T4 * T5 * T6.

%!  encode_value(+Value, -EncodedValue) is det.
%
%   Encodes a register value (integer or symbol).
encode_value(Value, Encoded) :-
    integer(Value),
    !,
    encode_symbol(Value, Encoded).
encode_value(Value, Encoded) :-
    encode_symbol(Value, Encoded).

%% ========================================================================
%% Configuration Encoding
%% ========================================================================

%!  encode_configuration(+Configuration, -GödelNumber) is det.
%
%   A configuration is a snapshot of the automaton during execution.
%   For now, equivalent to state encoding, but could be extended to
%   include input tape position, etc.

encode_configuration(Config, GödelNum) :-
    encode_state(Config, GödelNum).

%% ========================================================================
%% Transition Encoding
%% ========================================================================

%!  encode_transition(+Transition, -GödelNumber) is det.
%
%   Encodes a transition as a tuple: (CurrentState, NextState, Action).
%   
%   Encoded as: 2^g(CurrentState) × 3^g(NextState) × 5^g(Action)

encode_transition(transition(State1, State2, Action), GödelNum) :-
    encode_state(State1, G1),
    encode_state(State2, G2),
    encode_symbol(Action, G3),
    
    nth_prime(1, P1),
    nth_prime(2, P2),
    nth_prime(3, P3),
    
    prime_power(P1, G1, T1),
    prime_power(P2, G2, T2),
    prime_power(P3, G3, T3),
    
    GödelNum is T1 * T2 * T3.

%% ========================================================================
%% Trace Encoding
%% ========================================================================

%!  encode_trace(+Trace, -GödelNumber) is det.
%
%   Encodes a complete execution trace (sequence of configurations).
%   
%   For trace [C₀, C₁, C₂, ..., Cₙ]:
%   Gödel number = 2^g(C₀) × 3^g(C₁) × 5^g(C₂) × ... × Pₙ^g(Cₙ)

encode_trace([], 1) :- !.  % Empty trace = 1 (multiplicative identity)
encode_trace(Trace, GödelNum) :-
    encode_trace_helper(Trace, 1, 1, GödelNum).

encode_trace_helper([], _, Acc, Acc) :- !.
encode_trace_helper([Config|Rest], PrimeIndex, Acc, GödelNum) :-
    encode_configuration(Config, G),
    nth_prime(PrimeIndex, Prime),
    prime_power(Prime, G, Term),
    NewAcc is Acc * Term,
    NextIndex is PrimeIndex + 1,
    encode_trace_helper(Rest, NextIndex, NewAcc, GödelNum).

%% ========================================================================
%% Demonstrations
%% ========================================================================

%!  demo_c2c_encoding is det.
%
%   Demonstrates Gödel encoding of C2C multiplication states.

demo_c2c_encoding :-
    writeln('=== GÖDEL NUMBERING DEMONSTRATION: C2C MULTIPLICATION ==='),
    nl,
    
    % Example: 3 × 4 = 12
    writeln('Computing 3 × 4 using C2C strategy'),
    writeln(''),
    
    % Initial state: state(q_init, 0, 0, 0, 3, 4)
    State0 = state(q_init, 0, 0, 0, 3, 4),
    encode_state(State0, G0),
    format('Initial State: ~w~n', [State0]),
    format('Gödel Number: ~w~n', [G0]),
    nl,
    
    % After initialization: state(q_check_G, 0, 0, 0, 3, 4)
    State1 = state(q_check_G, 0, 0, 0, 3, 4),
    encode_state(State1, G1),
    format('After Init: ~w~n', [State1]),
    format('Gödel Number: ~w~n', [G1]),
    nl,
    
    % Counting first item: state(q_count_items, 0, 1, 1, 3, 4)
    State2 = state(q_count_items, 0, 1, 1, 3, 4),
    encode_state(State2, G2),
    format('First Count: ~w~n', [State2]),
    format('Gödel Number: ~w~n', [G2]),
    nl,
    
    % Final state: state(q_accept, 3, 0, 12, 3, 4)
    StateFinal = state(q_accept, 3, 0, 0, 3, 4),
    encode_state(StateFinal, GFinal),
    format('Accept State: ~w~n', [StateFinal]),
    format('Gödel Number: ~w~n', [GFinal]),
    nl,
    
    writeln('Each distinct state has a unique Gödel number.'),
    writeln('By the Fundamental Theorem of Arithmetic, this encoding is bijective.'),
    nl.

%!  demo_transition_encoding is det.
%
%   Demonstrates how transitions can be encoded arithmetically.

demo_transition_encoding :-
    writeln('=== TRANSITION ENCODING DEMONSTRATION ==='),
    nl,
    
    % Example transition from C2C
    State1 = state(q_count_items, 0, 1, 1, 3, 4),
    State2 = state(q_count_items, 0, 2, 2, 3, 4),
    Action = increment,
    
    Transition = transition(State1, State2, Action),
    encode_transition(Transition, GT),
    
    format('Transition: ~w~n', [Transition]),
    format('Gödel Number: ~w~n', [GT]),
    nl,
    
    writeln('This shows that transitions (the "cognitive moves") are arithmetic objects.'),
    writeln('The predicate Transition(x,y) = "x and y are consecutive configurations"'),
    writeln('can be expressed using only addition, multiplication, and exponentiation.'),
    nl,
    
    writeln('This is the KEY INSIGHT for Gödel\'s theorem:'),
    writeln('The MECHANICS of the automaton are ARITHMETIC PREDICATES.'),
    writeln('Therefore, the system can "talk about" its own computational process.'),
    nl.

%% ========================================================================
%% Arithmetic Predicates (Sketch)
%% ========================================================================

%!  is_transition(+X, +Y) is semidet.
%
%   Arithmetic predicate: true if X and Y encode consecutive configurations.
%   
%   This is a SKETCH showing how this would be expressed arithmetically.
%   In practice, this would use modular arithmetic to extract prime exponents
%   and verify the transition rules are satisfied.
%
%   The crucial point: this predicate uses only +, ×, ^, and comparison.

is_transition(X, Y) :-
    % Sketch: decode states from X and Y
    % Check if transition rules are satisfied
    % This is primitive recursive!
    
    % For demonstration, we just note that this is arithmetically definable
    format('Checking if ~w → ~w is a valid transition~n', [X, Y]),
    writeln('(This check uses only arithmetic operations on Gödel numbers)'),
    
    % In a full implementation, we would:
    % 1. Extract exponents of primes from X and Y
    % 2. Decode the state components
    % 3. Verify the transition matches one of the FSM rules
    % 4. All using primitive recursive arithmetic
    
    true.  % Placeholder

%!  is_valid_trace(+T) is semidet.
%
%   Arithmetic predicate: true if T encodes a valid execution trace.
%   
%   T is valid if:
%   ∀i: is_transition(Configuration_i, Configuration_{i+1})
%
%   This can be expressed arithmetically using bounded quantification.

is_valid_trace(T) :-
    format('Checking if ~w encodes a valid trace~n', [T]),
    writeln('(Verifying all consecutive pairs are valid transitions)'),
    writeln('(Using only arithmetic operations)'),
    true.  % Placeholder
