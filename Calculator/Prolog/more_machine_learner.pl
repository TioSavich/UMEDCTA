% Filename: more_machine_learner.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The More Machine Learner - Extended for Normative Critique              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(more_machine_learner,
          [ critique_and_bootstrap/1
          % bootstrap_from_observation/1, learned_strategy/1 % (Omitted for brevity)
          ]).

% --- Foundational Abilities ---

% The system's "teacher" (the norms). Imports the (=>)/2 operator implicitly.
:- use_module(incompatibility_semantics, [proves/1, incoherent/1, set_domain/1, current_domain/1]).

% --- Normative Critique and Bootstrapping (The Brandomian Move) ---
% Example usage: ?- critique_and_bootstrap(minus(3, 5, -2)).

critique_and_bootstrap(Observation) :-
    writeln('\n--- Normative Critique Initiated ---'),
    % Deconstruct the observation, e.g., minus(3, 5, -2)
    Observation =.. [Op, A, B, Result],
    format('Observation: ~w(~w, ~w) = ~w~n', [Op, A, B, Result]),

    % 1. Initial Assessment: Check coherence with current norms.
    current_domain(D),
    format('Current Domain: ~w~n', [D]),
    (   justify_observation(Op, A, B, Result)
    ->  writeln('Observation is coherent with current norms.')
    ;   % 2. The Critique: Identify the incompatibility.
        writeln('\nObservation is INCOHERENT with current norms. Entering critique phase...'),
        (   identify_incompatibility(Op, A, B, Result, Incompatibility)
        ->  format('Identified Incompatibility: ~w~n', [Incompatibility]),

            % 3. The Proposal: Suggest a normative shift.
            (   propose_normative_shift(Incompatibility, ProposedDomain)
            ->  format('Proposed Normative Shift: Change domain to ~w~n', [ProposedDomain]),

                % 4. The Resolution: Adopt the new norm and re-assess.
                writeln('Adopting new norms...'),
                set_domain(ProposedDomain),
                (   justify_observation(Op, A, B, Result)
                ->  writeln('\nBootstrapping complete. Observation is now coherent.'),
                    writeln('--- Normative Critique Concluded ---\n')
                ;   writeln('ERROR: Normative shift failed.')
                )
            ;   writeln('ERROR: Cannot propose a normative shift.')
            )
        ;   writeln('ERROR: Failed to identify the specific incompatibility.')
        )
    ).

% --- The Critique Logic ---

% Can the semantics prove this specific arithmetic fact? (Using Objective domain 'o')
justify_observation(Op, A, B, Result) :-
    Fact =.. [Op, A, B, Result],
    proves([] => [o(Fact)]).

% Specific critique for the subtraction limitation in Natural Numbers (N).
identify_incompatibility(minus, A, B, Result, incompatibility(limited_subtraction, domain(n))) :-
    current_domain(n),
    Result < 0, A < B,
    % Verify the core issue: Asserting the existence of the result is incoherent in N.
    % We use the normative domain 'n' for the existence claim, relying on the optimized check in the semantics.
    incoherent([n(obj_coll(minus(A,B,_)))]).

% --- The Resolution Logic ---
propose_normative_shift(incompatibility(limited_subtraction, domain(n)), z).