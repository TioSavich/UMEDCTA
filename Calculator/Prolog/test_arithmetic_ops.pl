/** <module> Test Arithmetic Operations
 *
 * Tests basic arithmetic operations in the Hermeneutic Calculator
 * to verify it meets requirements for Gödel's incompleteness theorem.
 */
:- module(test_arithmetic_ops, [run_arithmetic_tests/0]).

:- use_module(smr_mult_c2c).
:- use_module(sar_add_cobo).
:- use_module(grounded_arithmetic).

%! run_arithmetic_tests is det.
%
%  Runs tests to verify the HC implements addition and multiplication.
run_arithmetic_tests :-
    writeln('=== TESTING ARITHMETIC OPERATIONS FOR GÖDEL COMPLETENESS ==='),
    nl,
    test_addition,
    nl,
    test_multiplication,
    nl,
    test_grounded_arithmetic,
    nl,
    writeln('=== ARITHMETIC TESTS COMPLETE ===').

test_addition :-
    writeln('Test 1: Addition (COBO Strategy)'),
    writeln('  Testing: 7 + 5'),
    run_cobo(7, 5, Result1, History1),
    format('    Result: ~w~n', [Result1]),
    length(History1, Steps1),
    format('    Steps taken: ~w~n', [Steps1]),
    (Result1 =:= 12 -> writeln('    ✓ PASS') ; writeln('    ✗ FAIL')),
    
    writeln('  Testing: 23 + 17'),
    run_cobo(23, 17, Result2, History2),
    format('    Result: ~w~n', [Result2]),
    length(History2, Steps2),
    format('    Steps taken: ~w~n', [Steps2]),
    (Result2 =:= 40 -> writeln('    ✓ PASS') ; writeln('    ✗ FAIL')).

test_multiplication :-
    writeln('Test 2: Multiplication (C2C Strategy)'),
    writeln('  Testing: 3 * 4'),
    run_c2c(3, 4, Result1, History1),
    format('    Result: ~w~n', [Result1]),
    length(History1, Steps1),
    format('    Steps taken: ~w~n', [Steps1]),
    (Result1 =:= 12 -> writeln('    ✓ PASS') ; writeln('    ✗ FAIL')),
    
    writeln('  Testing: 5 * 7'),
    run_c2c(5, 7, Result2, History2),
    format('    Result: ~w~n', [Result2]),
    length(History2, Steps2),
    format('    Steps taken: ~w~n', [Steps2]),
    (Result2 =:= 35 -> writeln('    ✓ PASS') ; writeln('    ✗ FAIL')).

test_grounded_arithmetic :-
    writeln('Test 3: Grounded Arithmetic (Foundation Level)'),
    writeln('  Testing grounded addition: 5 + 3'),
    integer_to_recollection(5, Five),
    integer_to_recollection(3, Three),
    add_grounded(Five, Three, Sum),
    recollection_to_integer(Sum, SumInt),
    format('    Result: ~w~n', [SumInt]),
    (SumInt =:= 8 -> writeln('    ✓ PASS') ; writeln('    ✗ FAIL')),
    
    writeln('  Testing grounded multiplication: 5 * 3'),
    multiply_grounded(Five, Three, Product),
    recollection_to_integer(Product, ProductInt),
    format('    Result: ~w~n', [ProductInt]),
    (ProductInt =:= 15 -> writeln('    ✓ PASS') ; writeln('    ✗ FAIL')),
    
    writeln('  Testing grounded subtraction: 5 - 3'),
    subtract_grounded(Five, Three, Diff),
    recollection_to_integer(Diff, DiffInt),
    format('    Result: ~w~n', [DiffInt]),
    (DiffInt =:= 2 -> writeln('    ✓ PASS') ; writeln('    ✗ FAIL')),
    
    writeln('  Testing grounded division: 15 / 3'),
    integer_to_recollection(15, Fifteen),
    divide_grounded(Fifteen, Three, Quotient),
    recollection_to_integer(Quotient, QuotientInt),
    format('    Result: ~w~n', [QuotientInt]),
    (QuotientInt =:= 5 -> writeln('    ✓ PASS') ; writeln('    ✗ FAIL')).
