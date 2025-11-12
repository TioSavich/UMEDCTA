/** <module> Grounded Utilities for Base-10 Arithmetic
 *
 * This module provides utility predicates for working with base-10
 * decomposition in the grounded arithmetic framework. It implements
 * the embodied understanding of place-value structure.
 *
 * @author UMEDCA System
 * @license MIT
 */

:- module(grounded_utils, [
    base_decompose_grounded/4,
    base_recompose_grounded/4,
    decompose_base10/3
]).

:- use_module(grounded_arithmetic, [
    integer_to_recollection/2,
    recollection_to_integer/2,
    multiply_grounded/3,
    add_grounded/3,
    incur_cost/1
]).

%! base_decompose_grounded(+Number, +Base, -BasePart, -OnesPart) is det.
%
% Decomposes a number into its base part (multiples of Base) and ones part (remainder).
% For example, with Base=10: 27 → 20 (base part) + 7 (ones part)
%
% @param Number The number to decompose (as recollection)
% @param Base The base to use (as recollection, typically 10)
% @param BasePart The part that is a multiple of Base (as recollection)
% @param OnesPart The remainder (as recollection)
%
base_decompose_grounded(Number, Base, BasePart, OnesPart) :-
    incur_cost(base_decomposition),

    % Convert to integers for calculation (transition implementation)
    recollection_to_integer(Number, N),
    recollection_to_integer(Base, B),

    % Decompose
    BasePartInt is (N // B) * B,
    OnesPartInt is N mod B,

    % Convert back to recollections
    integer_to_recollection(BasePartInt, BasePart),
    integer_to_recollection(OnesPartInt, OnesPart).

%! base_recompose_grounded(+BasePart, +OnesPart, +Base, -Number) is det.
%
% Recomposes a number from its base part and ones part.
% For example: 20 (base part) + 7 (ones part) → 27
%
% @param BasePart The multiple of Base (as recollection)
% @param OnesPart The remainder (as recollection)
% @param Base The base being used (as recollection)
% @param Number The recomposed number (as recollection)
%
base_recompose_grounded(BasePart, OnesPart, _Base, Number) :-
    incur_cost(base_recomposition),
    add_grounded(BasePart, OnesPart, Number).

%! decompose_base10(+Number, -Tens, -Ones) is det.
%
% Convenience predicate for base-10 decomposition.
% Decomposes a number into tens and ones.
%
% @param Number The number to decompose (as recollection)
% @param Tens The tens part (as recollection)
% @param Ones The ones part (as recollection)
%
decompose_base10(Number, Tens, Ones) :-
    integer_to_recollection(10, Base10),
    base_decompose_grounded(Number, Base10, Tens, Ones).
