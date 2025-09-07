#!/bin/bash

# Test sar_add_cobo
echo "Testing sar_add_cobo..."
swipl -s - <<EOF
:- use_module('Calculator/Prolog/sar_add_cobo').
:- run_cobo(46, 37, Sum, _), (Sum =:= 83 -> writeln('sar_add_cobo PASSED') ; writeln('sar_add_cobo FAILED')).
:- halt.
EOF

# Test sar_sub_decomposition
echo "Testing sar_sub_decomposition..."
swipl -s - <<EOF
:- use_module('Calculator/Prolog/sar_sub_decomposition').
:- run_decomposition(45, 27, R1, _), (R1 =:= 18 -> writeln('sar_sub_decomposition PASSED (1)') ; writeln('sar_sub_decomposition FAILED (1)')).
:- run_decomposition(48, 23, R2, _), (R2 =:= 25 -> writeln('sar_sub_decomposition PASSED (2)') ; writeln('sar_sub_decomposition FAILED (2)')).
:- halt.
EOF

# Test sar_add_chunking
echo "Testing sar_add_chunking..."
swipl -s - <<EOF
:- use_module('Calculator/Prolog/sar_add_chunking').
:- run_chunking(46, 37, Sum, _), (Sum =:= 83 -> writeln('sar_add_chunking PASSED') ; writeln('sar_add_chunking FAILED')).
:- halt.
EOF

# Test sar_add_rmb
echo "Testing sar_add_rmb..."
swipl -s - <<EOF
:- use_module('Calculator/Prolog/sar_add_rmb').
:- run_rmb(8, 5, Sum, _), (Sum =:= 13 -> writeln('sar_add_rmb PASSED') ; writeln('sar_add_rmb FAILED')).
:- halt.
EOF

# Test sar_add_rounding
echo "Testing sar_add_rounding..."
swipl -s - <<EOF
:- use_module('Calculator/Prolog/sar_add_rounding').
:- run_rounding(8, 5, R1, _), (R1 =:= 13 -> writeln('sar_add_rounding PASSED (1)') ; writeln('sar_add_rounding FAILED (1)')).
:- run_rounding(46, 37, R2, _), (R2 =:= 83 -> writeln('sar_add_rounding PASSED (2)') ; writeln('sar_add_rounding FAILED (2)')).
:- halt.
EOF

# Test sar_sub_cobo_missing_addend
echo "Testing sar_sub_cobo_missing_addend..."
swipl -s - <<EOF
:- use_module('Calculator/Prolog/sar_sub_cobo_missing_addend').
:- run_cobo_ma(94, 65, R, _), (R =:= 29 -> writeln('sar_sub_cobo_missing_addend PASSED') ; writeln('sar_sub_cobo_missing_addend FAILED')).
:- halt.
EOF

# Test sar_sub_cbbo_take_away
echo "Testing sar_sub_cbbo_take_away..."
swipl -s - <<EOF
:- use_module('Calculator/Prolog/sar_sub_cbbo_take_away').
:- run_cbbo_ta(94, 65, R, _), (R =:= 29 -> writeln('sar_sub_cbbo_take_away PASSED') ; writeln('sar_sub_cbbo_take_away FAILED')).
:- halt.
EOF

# Test sar_sub_chunking_a
echo "Testing sar_sub_chunking_a..."
swipl -s - <<EOF
:- use_module('Calculator/Prolog/sar_sub_chunking_a').
:- run_chunking_a(400, 294, R, _), (R =:= 106 -> writeln('sar_sub_chunking_a PASSED') ; writeln('sar_sub_chunking_a FAILED')).
:- halt.
EOF

# Test sar_sub_chunking_b
echo "Testing sar_sub_chunking_b..."
swipl -s - <<EOF
:- use_module('Calculator/Prolog/sar_sub_chunking_b').
:- run_chunking_b(400, 294, R, _), (R =:= 106 -> writeln('sar_sub_chunking_b PASSED') ; writeln('sar_sub_chunking_b FAILED')).
:- halt.
EOF

# Test sar_sub_chunking_c
echo "Testing sar_sub_chunking_c..."
swipl -s - <<EOF
:- use_module('Calculator/Prolog/sar_sub_chunking_c').
:- run_chunking_c(400, 294, R, _), (R =:= 106 -> writeln('sar_sub_chunking_c PASSED') ; writeln('sar_sub_chunking_c FAILED')).
:- halt.
EOF

# Test sar_sub_rounding
echo "Testing sar_sub_rounding..."
swipl -s - <<EOF
:- use_module('Calculator/Prolog/sar_sub_rounding').
:- run_sub_rounding(84, 29, R, _), (R =:= 55 -> writeln('sar_sub_rounding PASSED') ; writeln('sar_sub_rounding FAILED')).
:- halt.
EOF

# Test sar_sub_sliding
echo "Testing sar_sub_sliding..."
swipl -s - <<EOF
:- use_module('Calculator/Prolog/sar_sub_sliding').
:- run_sliding(73, 47, R, _), (R =:= 26 -> writeln('sar_sub_sliding PASSED') ; writeln('sar_sub_sliding FAILED')).
:- halt.
EOF

# Test smr_div_cbo
echo "Testing smr_div_cbo..."
swipl -s - <<EOF
:- use_module('Calculator/Prolog/smr_div_cbo').
:- run_cbo_div(32, 8, 10, Q, R), (Q =:= 4, R=:=0 -> writeln('smr_div_cbo PASSED') ; writeln('smr_div_cbo FAILED')).
:- halt.
EOF

# Test smr_div_dealing_by_ones
echo "Testing smr_div_dealing_by_ones..."
swipl -s - <<EOF
:- use_module('Calculator/Prolog/smr_div_dealing_by_ones').
:- run_dealing_by_ones(12, 4, Q, _), (Q =:= 3 -> writeln('smr_div_dealing_by_ones PASSED') ; writeln('smr_div_dealing_by_ones FAILED')).
:- halt.
EOF

# Test smr_div_idp
echo "Testing smr_div_idp..."
swipl -s - <<EOF
:- use_module('Calculator/Prolog/smr_div_idp').
:- KB = [40-5, 16-2, 8-1], run_idp(56, 8, KB, Q, R), (Q =:= 7, R=:=0 -> writeln('smr_div_idp PASSED') ; writeln('smr_div_idp FAILED')).
:- halt.
EOF

# Test smr_div_ucr
echo "Testing smr_div_ucr..."
swipl -s - <<EOF
:- use_module('Calculator/Prolog/smr_div_ucr').
:- run_ucr(56, 8, Q, _), (Q =:= 7 -> writeln('smr_div_ucr PASSED') ; writeln('smr_div_ucr FAILED')).
:- halt.
EOF

# Test smr_mult_c2c
echo "Testing smr_mult_c2c..."
swipl -s - <<EOF
:- use_module('Calculator/Prolog/smr_mult_c2c').
:- run_c2c(3, 6, T, _), (T =:= 18 -> writeln('smr_mult_c2c PASSED') ; writeln('smr_mult_c2c FAILED')).
:- halt.
EOF

# Test smr_mult_cbo
echo "Testing smr_mult_cbo..."
swipl -s - <<EOF
:- use_module('Calculator/Prolog/smr_mult_cbo').
:- run_cbo_mult(7, 9, 10, T, _), (T =:= 63 -> writeln('smr_mult_cbo PASSED') ; writeln('smr_mult_cbo FAILED')).
:- halt.
EOF

# Test smr_mult_commutative_reasoning
echo "Testing smr_mult_commutative_reasoning..."
swipl -s - <<EOF
:- use_module('Calculator/Prolog/smr_mult_commutative_reasoning').
:- run_commutative_mult(10, 7, T, _), (T =:= 70 -> writeln('smr_mult_commutative_reasoning PASSED') ; writeln('smr_mult_commutative_reasoning FAILED')).
:- halt.
EOF

# Test smr_mult_dr
echo "Testing smr_mult_dr..."
swipl -s - <<EOF
:- use_module('Calculator/Prolog/smr_mult_dr').
:- run_dr(5, 7, T, _), (T =:= 35 -> writeln('smr_mult_dr PASSED') ; writeln('smr_mult_dr FAILED')).
:- halt.
EOF
