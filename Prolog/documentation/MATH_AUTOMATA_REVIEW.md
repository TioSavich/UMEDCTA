# Math Automata Review & Integration Plan

**Date**: November 3, 2025
**Status**: Analysis Complete, Integration Pending

---

## Files Reviewed

### Fraction Files (4 files → 2 needed)

1. **jason.pl** (1.3K) - Small, imports grounded operations
   - Depends on external modules: `grounded_ens_operations`, `normalization`, `grounded_arithmetic`
   - Implements partitive fractional scheme (PFS)

2. **jason_backup.pl** (11K) - Complete standalone implementation
3. **jason_temp.pl** (11K) - **IDENTICAL to jason_backup.pl** ✅ Can delete

4. **fraction_semantics.pl** (2.6K)
   - Depends on `composition_engine`, `grounded_arithmetic`
   - Defines equivalence rules: grouping and composition

**Verdict**:
- Delete `jason_temp.pl` (duplicate)
- Keep `jason_backup.pl` as canonical version (rename to `fractions.pl`)
- Keep `fraction_semantics.pl` (provides equivalence rules)
- Delete or mark `jason.pl` as deprecated (incomplete, missing dependencies)

---

### Counting Files (2 files)

1. **counting2.pl** (~200 lines)
   - Implements Deterministic Pushdown Automaton (DPDA) for counting
   - Models place-value system with carry (units → tens → hundreds)
   - Uses stack representation: `['U5', 'T2', 'H1', '#']` for 125

2. **counting_on_back.pl**
   - *(Not yet reviewed in detail)*

**Assessment**:
- `counting2.pl` is a well-implemented DPDA
- Uses standard FSM pattern with transition rules
- **Can integrate** with PML by wrapping in modal context

---

### SAR Files (Single-Addend Reasoning) (10 files)

Pattern: `sar_[operation]_[strategy].pl`

Examples reviewed:
1. **sar_add_chunking.pl** (~300 lines)
   - Strategy: Decompose B into tens+ones, add sequentially
   - Uses grounded arithmetic operations
   - **Already imports incompatibility_semantics!** ✅
   - Emits modal signals: `s(exp_poss(initiating_chunking_strategy))`

**Other SAR files** (not yet reviewed):
- `sar_add_cobo.pl`
- `sar_add_rmb.pl`
- `sar_add_rounding.pl`
- `sar_sub_cbbo_take_away.pl`
- `sar_sub_chunking_a.pl`
- `sar_sub_chunking_b.pl`
- `sar_sub_chunking_c.pl`
- More...

**Assessment**:
- These are **already partially integrated** with modal logic!
- Use `s/1`, `comp_nec/1`, `exp_poss/1` operators
- Import `incompatibility_semantics`
- **Problem**: They depend on external modules that don't exist in current codebase:
  - `grounded_arithmetic`
  - `grounded_utils`
  - `fsm_engine`
  - `composition_engine`
  - etc.

---

### SMR Files (Single-Multiplier Reasoning)

Pattern: `smr_*.pl`

*(Not yet reviewed)*

---

## Dependency Analysis

### Missing Modules

The existing SAR/SMR/fraction files depend on a **grounded arithmetic system** that is not present in the current codebase:

```
MISSING:
├── grounded_arithmetic.pl
│   ├── incur_cost/1
│   ├── add_grounded/3
│   ├── subtract_grounded/3
│   ├── multiply_grounded/3
│   ├── integer_to_recollection/2
│   ├── recollection_to_integer/2
│   └── greater_than/2, smaller_than/2, etc.
│
├── grounded_ens_operations.pl
│   └── ens_partition/3
│
├── grounded_utils.pl
│   ├── base_decompose_grounded/4
│   └── base_recompose_grounded/4
│
├── fsm_engine.pl
│   └── run_fsm_with_base/5
│
├── composition_engine.pl
│   └── find_and_extract_copies/4
│
└── normalization.pl
    └── normalize/2
```

### What Exists in Current Codebase

```
PRESENT:
├── incompatibility_semantics.pl
│   ├── proves/4
│   ├── material_inference/3
│   └── Modal operators: s/1, o/1, n/1
│
├── automata.pl
│   ├── highlander/2
│   ├── generate_trace/1
│   └── nth_prime/2
│
└── dialectical_engine.pl
    └── run_fsm/4 (generic FSM runner)
```

---

## Integration Options

### Option 1: Complete Grounded Arithmetic System (High Effort)

**Implement all missing modules**:
- `grounded_arithmetic.pl` with recollection-based representation
- `grounded_ens_operations.pl` for partitioning
- `fsm_engine.pl` for strategy execution
- `composition_engine.pl` for fraction composition
- `normalization.pl` for result simplification

**Pros**:
- Preserves existing SAR/SMR code as-is
- Complete "grounded" arithmetic system (no built-in arithmetic)
- Faithful to original cognitive model

**Cons**:
- **Large implementation burden** (~1000+ lines of new code)
- Duplicate functionality (we already have arithmetic in Prolog)
- May not be necessary for book purposes

**Estimate**: 1-2 full days of work

---

### Option 2: Adapter Layer (Medium Effort) ✅ RECOMMENDED

**Create adapters** that map missing predicates to PML equivalents or standard Prolog:

```prolog
% grounded_arithmetic_adapter.pl
:- module(grounded_arithmetic, [
    incur_cost/1,
    add_grounded/3,
    integer_to_recollection/2,
    % ... etc
]).

% Map to PML resource tracking
incur_cost(Stage) :-
    Cost = 1,  % Or lookup based on Stage
    format('  [Cost: ~w for ~w]~n', [Cost, Stage]).

% Use standard arithmetic but wrap in grounded representation
add_grounded(recollection(A), recollection(B), recollection(C)) :-
    length(A, AInt),
    length(B, BInt),
    CInt is AInt + BInt,
    length(C, CInt).

integer_to_recollection(N, recollection(List)) :-
    length(List, N),
    maplist(=(t), List).

% ... etc
```

**Pros**:
- **Much faster** to implement (~200-300 lines)
- Reuses existing Prolog arithmetic
- Preserves SAR/SMR code structure
- Can progressively refine adapters if needed

**Cons**:
- Not "pure" grounded arithmetic
- Loses some cognitive fidelity

**Estimate**: 2-3 hours of work

---

### Option 3: Extract to Material Inferences (Low Effort)

**Extract the core strategies** and represent as material inferences (like we did with Lakoff/Brandom):

```prolog
% arithmetic_strategies.pl (extended)

% SAR: Chunking Strategy
incompatibility_semantics:material_inference(
    [s(problem(addition(A, B))),
     s(decompose(B, BasePart, OnesPart))],
    s(comp_nec(chunking_strategy(A, B, BasePart, OnesPart))),
    true
).

% Strategy deployment with prerequisites
pp_necessity(chunking, base_10_understanding).
pp_necessity(chunking, sequential_addition).
pp_necessity(chunking, place_value_concept).
```

**Pros**:
- **Fastest** approach
- Clean integration with existing arithmetic_strategies.pl
- Focuses on **meaning-use** analysis (what the strategies DO)
- Good for book purposes (demonstrates patterns)

**Cons**:
- **Loses executable strategies** (can't actually run chunking algorithm)
- Less faithful to original cognitive models

**Estimate**: 1 hour per strategy

---

## Recommendations

### Immediate Actions

1. **Delete duplicate**: Remove `jason_temp.pl` (identical to jason_backup.pl) ✅

2. **Rename for clarity**:
   - `jason_backup.pl` → `fractions_fsm.pl` (full implementation)
   - `jason.pl` → DELETED or marked deprecated

3. **Choose integration path**:
   - **For book/demo**: Option 3 (Extract to material inferences) ✅
   - **For research/completeness**: Option 2 (Adapter layer)

### Prioritized Integration

#### Phase 1: Counting (Simplest)
- `counting2.pl` already complete and self-contained
- Wrap in modal context
- Add as material inference to arithmetic_strategies.pl
- Write tests

#### Phase 2: Fractions (Medium)
- Use `fractions_fsm.pl` (the complete version)
- Create minimal adapters for missing predicates
- Integrate with incompatibility_semantics
- Write tests demonstrating partitive reasoning

#### Phase 3: SAR Strategies (Most Complex)
- Start with one strategy: `sar_add_chunking.pl`
- Create adapter layer for grounded_arithmetic
- Verify it runs
- Extract pattern for other SAR files

#### Phase 4: SMR Strategies
- Similar pattern to SAR
- Add as needed

---

## Code Quality Assessment

### Well-Structured Files ✅
- `jason_backup.pl`: Clean FSM implementation, good documentation
- `counting2.pl`: Clear DPDA model
- `sar_add_chunking.pl`: Good state machine structure

### Partial Integration ✅
- SAR files already use `incompatibility_semantics`
- Modal operators already present (`s/1`, `comp_nec/1`, etc.)
- Cost tracking via `incur_cost/1`

### Missing Infrastructure ⚠️
- No grounded arithmetic system
- No FSM engine (though we have dialectical_engine.pl)
- No composition/normalization utilities

### Duplicates Found ✅
- `jason_temp.pl` = `jason_backup.pl` (delete one)
- `jason.pl` is incomplete (delete or mark deprecated)

---

## Proposed File Structure (After Integration)

```
math/
├── load_math.pl
├── lakoff_metaphors.pl          # ✅ Done
├── arithmetic_strategies.pl     # ✅ Done (will extend)
│
├── counting_automata.pl         # ⭐ NEW (consolidate counting*.pl)
├── fraction_automata.pl         # ⭐ NEW (consolidate fractions)
├── fraction_semantics.pl        # ✅ Keep as-is
│
├── sar_strategies/              # ⭐ NEW subfolder
│   ├── sar_chunking.pl
│   ├── sar_rounding.pl
│   └── ...
│
├── adapters/                    # ⭐ NEW (Option 2)
│   ├── grounded_arithmetic.pl
│   ├── fsm_engine.pl
│   └── ...
│
└── legacy/                      # ⭐ OLD files moved here
    ├── jason.pl (deprecated)
    ├── jason_temp.pl (deleted)
    └── ...
```

---

## Next Steps

1. ✅ Delete `jason_temp.pl`
2. ✅ Rename `jason_backup.pl` → `fractions_fsm.pl`
3. Choose integration approach (recommend Option 2 for adapters)
4. Start with Phase 1: Counting integration
5. Create adapter stubs for grounded_arithmetic
6. Test one complete strategy end-to-end

---

## Summary

**Good News**:
- Code is well-structured
- Partial PML integration already exists
- Clear strategy patterns

**Challenge**:
- Missing grounded arithmetic infrastructure
- ~30 files to integrate

**Solution**:
- Adapter layer (Option 2) for fastest working integration
- Progressive extraction to material inferences (Option 3) for book
- Prioritize by simplicity (Counting → Fractions → SAR → SMR)

**Estimate for Complete Integration**:
- Adapter approach: 4-6 hours
- Full grounded system: 2-3 days
- Material inference extraction: 1-2 hours per strategy
