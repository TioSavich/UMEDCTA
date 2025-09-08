% Filename: interactive_ui.pl
% Description: Contains the interactive UI for the More Machine Learner.

:- module(interactive_ui, [start/0]).

:- use_module(more_machine_learner).

% --- Main Entry Point ---

start :-
    welcome_message,
    ask_to_load_knowledge,
    main_menu.

% --- Interactive UI Predicates ---

welcome_message :-
    nl,
    writeln('===================================================='),
    writeln('          Welcome to the More Machine Learner       '),
    writeln('===================================================='),
    writeln('All I can do is count, but I can learn from what you show me.'),
    nl.

ask_to_load_knowledge :-
    write('Do you want to load previously learned strategies? (y/n) > '),
    read_line_to_string(user_input, Response),
    (   (Response = "y" ; Response = "Y")
    ->  (   exists_file('learned_knowledge.pl')
        ->  writeln('Loading previously learned knowledge...'),
            consult('learned_knowledge.pl')
        ;   writeln('No saved knowledge file found.')
        )
    ;   writeln('Starting with a clean slate.')
    ).

main_menu :-
    nl,
    writeln('--- Main Menu ---'),
    writeln('1. Learn a new addition strategy (e.g., from 8+5=13)'),
    writeln('2. Critique a normative rule (e.g., from 3-5=-2)'),
    writeln('3. Show currently learned strategies'),
    writeln('4. Save learned strategies'),
    writeln('5. Exit'),
    write('> '),
    read_line_to_string(user_input, Choice),
    handle_menu_choice(Choice).

handle_menu_choice("1") :- !, run_learning_interaction, main_menu.
handle_menu_choice("2") :- !, run_critique_interaction, main_menu.
handle_menu_choice("3") :- !, show_learned_strategies, main_menu.
handle_menu_choice("4") :- !, save_knowledge, main_menu.
handle_menu_choice("5") :- !, writeln('Goodbye!'), nl.
handle_menu_choice(_) :- writeln('Invalid choice, please try again.'), main_menu.

run_learning_interaction :-
    nl,
    writeln('--- Learning a New Strategy ---'),
    writeln('Please provide a basic addition problem and its result.'),
    write('Example: 8+5=13'), nl,
    write('Problem > '),
    read_line_to_string(user_input, ProblemString),
    (   parse_problem(ProblemString, +(A,B), Result)
    ->  bootstrap_from_observation(+(A,B), Result)
    ;   writeln('Invalid problem format. Please use the format "A+B=C".')
    ).

run_critique_interaction :-
    nl,
    writeln('--- Critiquing a Norm ---'),
    writeln('Please provide a challenging subtraction problem.'),
    write('Example: 3-5=-2'), nl,
    write('Problem > '),
    read_line_to_string(user_input, ProblemString),
    (   parse_problem(ProblemString, -(A,B), Result)
    ->  critique_and_bootstrap(minus(A, B, Result))
    ;   writeln('Invalid problem format. Please use the format "A-B=C".')
    ).

show_learned_strategies :-
    nl,
    writeln('--- Learned Strategies ---'),
    (   current_predicate(learned_strategy/1)
    ->  listing(learned_strategy/1)
    ;   writeln('No strategies have been learned in this session.')
    ),
    nl.

% --- Parsing Helper ---
parse_problem(String, Term, Result) :-
    normalize_space(string(CleanString), String),
    atomic_list_concat(Parts, '=', CleanString),
    (   Parts = [Problem, ResultStr]
    ->  normalize_space(string(TrimmedResult), ResultStr),
        number_string(Result, TrimmedResult),
        (   atomic_list_concat([A_str, B_str], '+', Problem)
        ->  normalize_space(string(TrimmedA), A_str),
            normalize_space(string(TrimmedB), B_str),
            number_string(A, TrimmedA),
            number_string(B, TrimmedB),
            Term = +(A,B)
        ;   atomic_list_concat([A_str, B_str], '-', Problem)
        ->  normalize_space(string(TrimmedA), A_str),
            normalize_space(string(TrimmedB), B_str),
            number_string(A, TrimmedA),
            number_string(B, TrimmedB),
            Term = -(A,B)
        ;   fail
        )
    ;   fail
    ).
