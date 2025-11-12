# Math Automata Integration - Status & Action Plan

**Date**: November 3, 2025
**Status**: ⚠️ Ready for Integration Decision

---

## Executive Summary

I've reviewed all the counting, SAR, SMR, and fraction files in the math/ directory. Here's what I found:

### ✅ Good News

1. **Code Quality**: Files are well-structured with clear FSM patterns
2. **Partial Integration**: SAR files already use `incompatibility_semantics` and modal operators
3. **Duplicates Removed**: Cleaned up jason files (removed duplicate, renamed for clarity)
4. **Documentation Organized**: All .md files moved to documentation/ folder

### ⚠️ Challenge

**The files depend on a complete "grounded arithmetic" system that doesn't exist in the current codebase.**

Missing modules:
- `grounded_arithmetic.pl` (~500 lines needed)
- `grounded_ens_operations.pl` (~200 lines)
- `fsm_engine.pl` (~300 lines)
- `composition_engine.pl` (~200 lines)
- `normalization.pl` (~100 lines)

**Total missing code**: ~1300 lines

---

## Files Cleaned Up

### Before
```
math/
├── jason.pl (1.3K, incomplete)
├── jason_backup.pl (11K, complete)
├── jason_temp.pl (11K, DUPLICATE ❌)
└── fraction_semantics.pl (2.6K)
```

### After
```
math/
├── jason_deprecated.pl (1.3K, marked as deprecated)
├── fractions_fsm.pl (11K, ✅ renamed from jason_backup.pl)
└── fraction_semantics.pl (2.6K, ✅ kept)
```

**Actions taken**:
- ✅ Deleted `jason_temp.pl` (identical duplicate)
- ✅ Renamed `jason_backup.pl` → `fractions_fsm.pl`
- ✅ Renamed `jason.pl` → `jason_deprecated.pl`

---

## Integration Options

I've identified **three approaches**, ranked by effort:

### Option 1: Complete Grounded System (High Effort - 2-3 days)

**Build the entire missing infrastructure**

Implement:
- Full recollection-based arithmetic (no built-in `is/2`)
- ENS (Explicitly Nested Number Sequence) operations
- Custom FSM engine with base-10 tracking
- Composition and normalization engines

**Pros**:
- Preserves all existing SAR/SMR code
- Complete cognitive fidelity
- "Pure" grounded arithmetic system

**Cons**:
- **Large time investment**
- Duplicates Prolog's built-in arithmetic
- May be overkill for book purposes

---

### Option 2: Adapter Layer (Medium Effort - 4-6 hours) ⭐ RECOMMENDED

**Create thin adapters that map missing predicates to PML or standard Prolog**

Example:
```prolog
% math/adapters/grounded_arithmetic.pl
:- module(grounded_arithmetic, [
    incur_cost/1,
    add_grounded/3,
    integer_to_recollection/2,
    recollection_to_integer/2
]).

% Map cost tracking to PML resource system
incur_cost(Stage) :-
    Cost = 1,
    format('  [Incurred cost: ~w for stage: ~w]~n', [Cost, Stage]).

% Wrap standard arithmetic in recollection representation
add_grounded(recollection(A), recollection(B), recollection(C)) :-
    length(A, AInt),
    length(B, BInt),
    CInt is AInt + BInt,
    length(C, CInt),
    maplist(=(t), C).

integer_to_recollection(N, recollection(List)) :-
    length(List, N),
    maplist(=(t), List).

recollection_to_integer(recollection(List), N) :-
    length(List, N).
```

**Pros**:
- **Fast to implement** (few hours)
- Preserves SAR/SMR structure
- Gets strategies working quickly
- Can refine later if needed

**Cons**:
- Not "pure" grounded arithmetic
- Uses Prolog's built-in `is/2` under the hood

**Recommended modules to create**:
1. `adapters/grounded_arithmetic.pl` (~200 lines)
2. `adapters/grounded_utils.pl` (~100 lines)
3. `adapters/fsm_engine.pl` (~150 lines)
4. `adapters/composition_engine.pl` (~100 lines)
5. `adapters/normalization.pl` (~50 lines)

**Total**: ~600 lines (manageable)

---

### Option 3: Extract to Material Inferences (Low Effort - 1-2 hours per strategy)

**Don't run the strategies—just represent their patterns**

Example:
```prolog
% Extend arithmetic_strategies.pl

% Chunking Strategy
incompatibility_semantics:material_inference(
    [s(problem(addition(A, B))),
     s(base_10_structure),
     s(decompose(B, Tens, Ones))],
    s(comp_nec(apply_chunking(A, Tens, Ones))),
    true
).

pp_necessity(chunking, base_10_understanding).
pp_necessity(chunking, sequential_addition).
pp_necessity(chunking, decomposition_ability).

pp_sufficiency(chunking, base_recognition).
pp_sufficiency(chunking, mental_addition).
```

**Pros**:
- **Fastest** approach
- Clean integration with existing `arithmetic_strategies.pl`
- Focuses on **meaning-use** (what strategies DO)
- Perfect for book/theory purposes

**Cons**:
- **Doesn't execute strategies** (no actual algorithm runs)
- Loses executable/demo capability

---

## My Recommendation

**Option 2: Adapter Layer** ⭐

### Why?

1. **Balance**: Not too much work, not too little functionality
2. **Executable**: Strategies actually run and can be demonstrated
3. **Extensible**: Can refine adapters or swap for full grounded system later
4. **Integration**: Works with existing PML critique mechanisms
5. **Time**: 4-6 hours is reasonable investment

### Implementation Plan

#### Phase 1: Core Adapters (2 hours)
Create `math/adapters/grounded_arithmetic.pl`:
- `incur_cost/1` → log to console or track in PML resources
- `integer_to_recollection/2` → convert int ↔ list representation
- `add_grounded/3`, `subtract_grounded/3`, `multiply_grounded/3` → wrap standard arithmetic

#### Phase 2: Utility Adapters (1 hour)
Create `math/adapters/grounded_utils.pl`:
- `base_decompose_grounded/4` → split into tens/ones
- `base_recompose_grounded/4` → recombine

Create `math/adapters/fsm_engine.pl`:
- `run_fsm_with_base/5` → wrapper around `dialectical_engine:run_fsm/4`

#### Phase 3: Fraction Adapters (1 hour)
Create `math/adapters/composition_engine.pl`:
- `find_and_extract_copies/4` → list manipulation

Create `math/adapters/normalization.pl`:
- `normalize/2` → simplify representations

Create `math/adapters/grounded_ens_operations.pl`:
- `ens_partition/3` → partition units

#### Phase 4: Integration Testing (1-2 hours)
Test one complete strategy:
1. Load adapters
2. Load `counting2.pl` (simplest)
3. Run `run_counter(25, Result)`
4. Verify output
5. Repeat with `fractions_fsm.pl`
6. Repeat with `sar_add_chunking.pl`

---

## Alternative: Hybrid Approach

**Combine Options 2 and 3**:
- Create **adapters** for counting and fractions (they're simpler)
- **Extract patterns** from SAR/SMR to material inferences (there are many)

This gives you:
- **Executable demos** (counting, fractions)
- **Theoretical analysis** (SAR/SMR patterns)
- **Balanced effort** (6-8 hours total)

---

## Current Status

### ✅ Completed
- Documentation folder created
- All .md files moved to documentation/
- Duplicate jason files removed
- Files renamed for clarity
- Comprehensive review document created

### ⏳ Pending Decision
**Which integration approach do you want?**
1. Full grounded system (2-3 days)
2. Adapter layer (4-6 hours) ⭐
3. Material inference extraction (1-2 hours per strategy)
4. Hybrid (adapters + extraction, 6-8 hours)

---

## Files Currently in Math/

### Working (Already Integrated)
- `lakoff_metaphors.pl` ✅
- `arithmetic_strategies.pl` ✅
- `load_math.pl` ✅
- `lakoff_brandom_test.pl` ✅

### Needs Integration
**Counting** (2 files):
- `counting2.pl` - DPDA for counting 0→N
- `counting_on_back.pl` - Counting backwards

**Fractions** (2 files):
- `fractions_fsm.pl` - Partitive/Composition schemes
- `fraction_semantics.pl` - Equivalence rules

**SAR** (10+ files):
- `sar_add_chunking.pl`
- `sar_add_cobo.pl`
- `sar_add_rmb.pl`
- `sar_add_rounding.pl`
- `sar_sub_*.pl` (multiple)

**SMR** (unknown count):
- `smr_*.pl` (not yet reviewed)

**Deprecated**:
- `jason_deprecated.pl` (can delete)

---

## Recommendation Summary

**For book purposes with working demos**: Option 2 (Adapter Layer)

**Implementation priority**:
1. Counting (simplest, self-contained)
2. Fractions (moderate, clear cognitive model)
3. SAR strategies (complex, many files)

**Time estimate**:
- Adapters: 4-6 hours
- Testing: 1-2 hours
- Total: 5-8 hours for complete integration

**Deliverable**:
- Working counting automaton
- Working fraction reasoning
- At least 2-3 SAR strategies executable
- All hooked into PML critique mechanisms

---

## Next Steps (Awaiting Your Decision)

**If Option 2 (Adapter Layer)**:
1. Create `math/adapters/` directory
2. Implement grounded_arithmetic.pl
3. Implement grounded_utils.pl
4. Test with counting2.pl
5. Extend to fractions
6. Add SAR strategies incrementally

**If Option 3 (Material Inferences)**:
1. Review each strategy for core pattern
2. Extract PP-necessities/sufficiencies
3. Add to arithmetic_strategies.pl
4. Write tests for pattern recognition
5. Document meaning-use analysis

**If Hybrid**:
1. Adapters for counting + fractions
2. Extract patterns for SAR/SMR
3. Best of both worlds

---

## Questions for You

1. **Do you want executable strategies** (Option 2), or **theoretical analysis only** (Option 3)?
2. **How much time can you allocate** to this integration?
3. **Which is more important for the book**: Working demos or comprehensive coverage?
4. **Should I prioritize** getting 2-3 strategies working perfectly, or documenting all strategies at a high level?

---

**Ready to proceed once you provide direction.**
