# PML Framework Reorganization - Complete

**Date**: November 3, 2025
**Status**: ✅ COMPLETE - All tests passing, domain separation clean

---

## What Was Done

### 1. Domain Separation

**Before**: Math-specific content mixed with core framework in root directory

**After**: Clean separation
```
Prolog/                      # Core framework (domain-agnostic)
  ├── 10 core .pl modules
  ├── README.md
  ├── tests/                 # Core tests only
  │   ├── simple_test.pl     # 10 tests
  │   ├── critique_test.pl   # 7 tests
  │   └── *.md               # Core documentation
  └── math/                  # Math domain instantiation
      ├── load_math.pl       # Math-specific loader
      ├── lakoff_metaphors.pl
      ├── arithmetic_strategies.pl
      ├── lakoff_brandom_test.pl  # 29 tests
      ├── README.md          # Math-specific docs
      └── *.pl               # Legacy strategy files
```

### 2. Files Moved

**From root → math/**:
- `lakoff_metaphors.pl` (350 lines)
- `arithmetic_strategies.pl` (300 lines)
- `LAKOFF_BRANDOM_README.md`

**From tests/ → math/**:
- `lakoff_brandom_test.pl` (320 lines)
- `LAKOFF_BRANDOM_INTEGRATION.md`
- `lakoff_brandom_results.txt`

**Deleted**:
- `math/LK_RB_Content_Extract/` - Content successfully extracted into Prolog

### 3. New Files Created

**Core**:
- `README.md` - Framework overview and usage guide

**Math Domain**:
- `math/load_math.pl` - Loader for core + math content
- `math/README.md` - Math domain documentation and template for other domains

### 4. Files Updated

**Core**:
- `load.pl` - Removed math-specific imports, now loads core only

**Math**:
- `lakoff_brandom_test.pl` - Updated to use `load_math.pl`

---

## Current Structure

### Root Directory (Domain-Agnostic Core)

```
Prolog/
├── README.md                         ⭐ NEW - Framework guide
├── load.pl                           ✏️  UPDATED - Core only
│
├── Core Modules (10 files)
│   ├── pml_operators.pl
│   ├── utils.pl
│   ├── incompatibility_semantics.pl  # 11K lines
│   ├── automata.pl
│   ├── semantic_axioms.pl
│   ├── pragmatic_axioms.pl
│   ├── intersubjective_praxis.pl
│   ├── critique.pl
│   └── dialectical_engine.pl
│
└── tests/                            # Core tests only
    ├── simple_test.pl                # 10 tests
    ├── core_test.pl
    ├── critique_test.pl              # 7 tests
    ├── TEST_SUMMARY.md
    └── CRITIQUE_IMPLEMENTATION.md
```

### Math Domain Directory

```
math/
├── README.md                         ⭐ NEW - Math domain guide
├── load_math.pl                      ⭐ NEW - Math loader
│
├── Content Modules
│   ├── lakoff_metaphors.pl           ⬅️  MOVED from root
│   └── arithmetic_strategies.pl      ⬅️  MOVED from root
│
├── Tests
│   ├── lakoff_brandom_test.pl        ⬅️  MOVED from tests/
│   └── lakoff_brandom_results.txt    ⬅️  MOVED from tests/
│
├── Documentation
│   ├── LAKOFF_BRANDOM_INTEGRATION.md ⬅️  MOVED from tests/
│   └── LAKOFF_BRANDOM_README.md      ⬅️  MOVED from root
│
└── Legacy Strategy Files (~20 files)
    ├── sar_*.pl                      # To be integrated
    ├── smr_*.pl                      # To be integrated
    └── counting*.pl, jason*.pl, etc.
```

---

## Test Results

### ✅ All Tests Passing

**Core Framework** (17/17):
- Simple tests: 10/10 ✅
- Critique tests: 7/7 ✅

**Math Domain** (29/29):
- Grounding metaphors: 3/3 ✅
- BMI metaphors: 3/3 ✅
- BMI pathologies: 3/3 ✅
- Arithmetic strategies: 4/4 ✅
- PP-necessities: 4/4 ✅
- LX-relations: 3/3 ✅
- PML dynamics: 4/4 ✅
- ORR cycle: 3/3 ✅
- Conceptual blends: 2/2 ✅

**Total**: 46/46 tests passing ✅

### ✅ No Regressions

- All core tests still pass after reorganization
- All math tests still pass from new location
- Load times unchanged
- No broken imports

---

## Usage

### Core Framework Only

```bash
# Load core
swipl load.pl

# Run core tests
cd tests
swipl -g run_tests -t halt simple_test.pl       # 10 tests
swipl -g run_all_tests -t halt critique_test.pl # 7 tests
```

### Core + Math Domain

```bash
# Load with math content
cd math
swipl load_math.pl

# Run math tests
swipl -g run_all_tests -t halt lakoff_brandom_test.pl  # 29 tests
```

---

## Benefits of Reorganization

### 1. **Clean Domain Separation**

The core framework is now truly **domain-agnostic**:
- No math-specific imports in `load.pl`
- No math-specific vocabulary in core modules
- Math content isolated in `math/` directory

This makes it easy to create other domain instantiations (physics, biology, law, etc.).

### 2. **Clear Dependency Structure**

```
Core Framework (no domain knowledge)
    ↓
Domain Content (math/, future: physics/, etc.)
```

Dependencies flow one way: domains depend on core, core depends on nothing.

### 3. **Modular Testing**

- **Core tests** verify framework mechanisms without domain content
- **Domain tests** verify content works with framework mechanisms
- Tests organized by scope (not mixed together)

### 4. **Template for Other Domains**

The `math/` directory structure serves as a **template**:

```
Prolog/[your_domain]/
├── load_[domain].pl           # Loads core + your content
├── [domain]_content.pl        # Material inferences
├── [domain]_strategies.pl     # Practices and patterns
├── [domain]_test.pl           # Domain-specific tests
└── README.md                  # Domain documentation
```

### 5. **Documentation Clarity**

- Root `README.md`: Framework overview (for all users)
- `math/README.md`: Math-specific guide (for math users)
- Test documentation with appropriate scope

---

## Implementation Notes

### Load Chain for Math Domain

```
math/load_math.pl
  ↓ [loads]
../load.pl
  ↓ [loads]
Core modules (pml_operators, incompatibility_semantics, etc.)
  ↓ [then load_math.pl loads]
lakoff_metaphors.pl, arithmetic_strategies.pl
```

### Module Imports

All modules use standard SWI-Prolog module system:
```prolog
:- module(lakoff_metaphors, []).
:- multifile incompatibility_semantics:material_inference/3.
```

Material inferences extend the core prover via multifile predicates.

### Testing Pattern

All test files use consistent pattern:
```prolog
run_test(Name, Goal) :-
    format('~n[TEST] ~w~n', [Name]),
    ( catch(Goal, Error, fail) ->
        writeln('  PASS')
    ;
        writeln('  FAIL')
    ).
```

---

## What Was NOT Changed

### Core Framework Unchanged

All 10 core modules remain **identical** in functionality:
- `incompatibility_semantics.pl` - Prover logic unchanged
- `critique.pl` - Critique mechanisms unchanged
- `dialectical_engine.pl` - ORR cycle unchanged
- Etc.

### Math Content Unchanged

The math modules (`lakoff_metaphors.pl`, `arithmetic_strategies.pl`) remain **functionally identical**:
- Same material inferences
- Same strategy definitions
- Same PP-necessities/sufficiencies
- Same pathology detections

Only their **location** changed (root → math/), not their content.

### Tests Unchanged

Test logic is **identical**:
- Same test cases
- Same assertions
- Same expected outcomes
- Only import paths updated (load.pl → load_math.pl)

---

## Status Summary

| Component | Status | Tests | Notes |
|-----------|--------|-------|-------|
| Core Framework | ✅ Complete | 17/17 | Domain-agnostic, no regressions |
| Math Domain | ✅ Complete | 29/29 | Cleanly separated, fully functional |
| Documentation | ✅ Complete | - | Root + domain READMEs |
| Test Organization | ✅ Complete | - | Core vs. domain separation |
| Domain Template | ✅ Ready | - | math/ serves as template |

---

## For the Book

This reorganization demonstrates a key design principle:

**Separation of framework from content.**

The PML Core Framework provides:
- Logic (modal operators, inference rules)
- Mechanisms (proof search, critique, trace)
- Architecture (ORR cycle, dialectical rhythm)

Domain instantiations provide:
- Material inferences ("axioms" for the domain)
- Practices (strategies, heuristics, patterns)
- Content for critique (pathologies, incoherences)

This separation means:
1. **The framework is reusable** across domains
2. **Domain content is swappable** (math, physics, law, etc.)
3. **Testing is modular** (test framework independently of content)
4. **Documentation is scoped** (framework guide vs. domain guide)

---

## Next Steps (Optional)

The math/ directory contains ~20 legacy strategy files (sar_*.pl, smr_*.pl, etc.) that could be progressively integrated:

1. **One file at a time**: Choose a strategy file (e.g., `sar_add_chunking.pl`)
2. **Extract the pattern**: Identify the core inference and prerequisites
3. **Add to arithmetic_strategies.pl**: Follow existing patterns
4. **Write tests**: Verify deployment and critique work
5. **Repeat**: Continue with other strategy files

But this is **not required** for the book—the current implementation is complete and demonstrates all key concepts.

---

## Conclusion

✅ **REORGANIZATION COMPLETE**

- Domain separation: Clean
- Tests: All passing (46/46)
- Documentation: Comprehensive
- Template: Ready for other domains
- No regressions: Core functionality preserved

The PML Core Framework is now a **truly domain-agnostic foundation** for embodied, pragmatic, dialectical reasoning, with **math as a working exemplar** of how to instantiate it for specific domains.

**This structure is publication-ready.**
