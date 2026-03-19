/** Test all strategies in hermeneutic calculator */

:- use_module(hermeneutic_calculator).

test_all :-
    writeln('=== Testing Addition Strategies ==='),
    test_addition,
    writeln(''),
    writeln('=== Testing Subtraction Strategies ==='),
    test_subtraction,
    writeln(''),
    writeln('=== Testing Multiplication Strategies ==='),
    test_multiplication,
    writeln(''),
    writeln('=== Testing Division Strategies ==='),
    test_division.

test_addition :-
    test_strategy(7, +, 5, 'COBO', 12),
    test_strategy(7, +, 5, 'Chunking', 12),
    test_strategy(7, +, 5, 'RMB', 12),
    test_strategy(7, +, 5, 'Rounding', 12).

test_subtraction :-
    test_strategy(12, -, 5, 'COBO (Missing Addend)', 7),
    test_strategy(12, -, 5, 'CBBO (Take Away)', 7),
    test_strategy(12, -, 5, 'Decomposition', 7),
    test_strategy(12, -, 5, 'Rounding', 7),
    test_strategy(12, -, 5, 'Sliding', 7),
    test_strategy(12, -, 5, 'Chunking A', 7),
    test_strategy(12, -, 5, 'Chunking B', 7),
    test_strategy(12, -, 5, 'Chunking C', 7).

test_multiplication :-
    test_strategy(3, *, 4, 'C2C', 12),
    test_strategy(3, *, 4, 'CBO', 12),
    test_strategy(3, *, 4, 'Commutative Reasoning', 12),
    test_strategy(3, *, 4, 'DR', 12).

test_division :-
    test_strategy(12, /, 3, 'CBO (Division)', 4),
    test_strategy(12, /, 3, 'Dealing by Ones', 4),
    % IDP requires learned multiplication facts, skip for now
    % test_strategy(12, /, 3, 'IDP', 4),
    test_strategy(12, /, 3, 'UCR', 4).

test_strategy(N1, Op, N2, Strategy, Expected) :-
    format('Testing: ~w ~w ~w using ~w... ', [N1, Op, N2, Strategy]),
    (   catch(calculate(N1, Op, N2, Strategy, Result, _), Error, (
            format('ERROR: ~w~n', [Error]),
            fail
        )),
        Result =:= Expected
    ->  writeln('PASS')
    ;   format('FAIL (got ~w, expected ~w)~n', [Result, Expected])
    ).
