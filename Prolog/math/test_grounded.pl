/** <module> Test Grounded Arithmetic Infrastructure
 *
 * Quick tests to verify the grounded arithmetic modules work correctly.
 */

:- use_module(grounded_arithmetic).
:- use_module(grounded_utils).
:- use_module(composition_engine).
:- use_module(normalization).
:- use_module(fsm_engine).

test_grounded_add :-
    writeln('\n[TEST] Grounded Addition'),
    integer_to_recollection(5, A),
    integer_to_recollection(3, B),
    add_grounded(A, B, C),
    recollection_to_integer(C, Result),
    format('  5 + 3 = ~w~n', [Result]),
    ( Result = 8 ->
        writeln('  PASS')
    ;
        writeln('  FAIL')
    ).

test_grounded_subtract :-
    writeln('\n[TEST] Grounded Subtraction'),
    integer_to_recollection(7, A),
    integer_to_recollection(3, B),
    subtract_grounded(A, B, C),
    recollection_to_integer(C, Result),
    format('  7 - 3 = ~w~n', [Result]),
    ( Result = 4 ->
        writeln('  PASS')
    ;
        writeln('  FAIL')
    ).

test_grounded_multiply :-
    writeln('\n[TEST] Grounded Multiplication'),
    integer_to_recollection(4, A),
    integer_to_recollection(3, B),
    multiply_grounded(A, B, C),
    recollection_to_integer(C, Result),
    format('  4 * 3 = ~w~n', [Result]),
    ( Result = 12 ->
        writeln('  PASS')
    ;
        writeln('  FAIL')
    ).

test_base_decompose :-
    writeln('\n[TEST] Base-10 Decomposition'),
    integer_to_recollection(27, N),
    decompose_base10(N, Tens, Ones),
    recollection_to_integer(Tens, TensInt),
    recollection_to_integer(Ones, OnesInt),
    format('  27 = ~w (tens) + ~w (ones)~n', [TensInt, OnesInt]),
    ( TensInt = 20, OnesInt = 7 ->
        writeln('  PASS')
    ;
        writeln('  FAIL')
    ).

test_comparison :-
    writeln('\n[TEST] Comparison Operations'),
    integer_to_recollection(5, A),
    integer_to_recollection(3, B),
    ( greater_than(A, B) ->
        writeln('  5 > 3: PASS')
    ;
        writeln('  5 > 3: FAIL')
    ),
    ( smaller_than(B, A) ->
        writeln('  3 < 5: PASS')
    ;
        writeln('  3 < 5: FAIL')
    ).

test_counting :-
    writeln('\n[TEST] Counting Automaton'),
    catch(
        ( use_module(counting2),
          counting2:run_counter(25, Result),
          format('  Counted to: ~w~n', [Result]),
          ( Result = 25 ->
              writeln('  PASS')
          ;
              writeln('  FAIL')
          )
        ),
        Error,
        ( format('  ERROR: ~w~n', [Error]),
          writeln('  FAIL')
        )
    ).

run_all_tests :-
    writeln(''),
    writeln('=== GROUNDED ARITHMETIC INFRASTRUCTURE TESTS ==='),
    test_grounded_add,
    test_grounded_subtract,
    test_grounded_multiply,
    test_base_decompose,
    test_comparison,
    test_counting,
    writeln(''),
    writeln('=== TESTS COMPLETE ==='),
    writeln('').

:- initialization(run_all_tests, main).
