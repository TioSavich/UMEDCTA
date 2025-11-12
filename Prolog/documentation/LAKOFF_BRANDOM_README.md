# Lakoff & Brandom Integration

This directory contains a working implementation of Lakoff's embodied mathematical metaphors and Brandom's meaning-use analysis within the PML (Polarized Modal Logic) Core Framework.

## Quick Start

```bash
# Run all tests (29 tests)
cd tests
swipl -g run_all_tests -t halt lakoff_brandom_test.pl

# Expected output:
# Passed: 29
# Failed: 0
```

## What's New

### Two New Modules

1. **[lakoff_metaphors.pl](lakoff_metaphors.pl)** (350 lines)
   - The 4Gs: Grounding metaphors for arithmetic (Object Collection, Object Construction, Measuring Stick, Motion Along Path)
   - The BMI: Basic Metaphor of Infinity and special cases (∞, ℕ, limits, infinitesimals)
   - Pathologies: Being↔Nothing, Zeno's Paradox, Russell's Paradox
   - Blends: Euler's formula (e^(iπ) + 1 = 0)

2. **[arithmetic_strategies.pl](arithmetic_strategies.pl)** (300 lines)
   - Three strategies: Sliding, Counting On, Rearranging to Make Bases
   - PP-necessities and PP-sufficiencies for each
   - LX-relations (elaboration hierarchies)
   - Integration with PML dynamics (compression/expansion/tension)

### Updated Files

- **[load.pl](load.pl)**: Now loads lakoff_metaphors and arithmetic_strategies
- **[tests/lakoff_brandom_test.pl](tests/lakoff_brandom_test.pl)**: Comprehensive test suite (29 tests)
- **[tests/LAKOFF_BRANDOM_INTEGRATION.md](tests/LAKOFF_BRANDOM_INTEGRATION.md)**: Full technical documentation

## Key Concepts

### Embodied Metaphors as Material Inferences

Lakoff's cognitive mappings become executable logic:

```prolog
% "Numbers Are Points on a Line"
incompatibility_semantics:material_inference(
    [s(point_on_line(X))],
    s(number(X)),
    true
).
```

### Strategies as Practices with Normative Structure

Brandomian strategies have prerequisites and deployment conditions:

```prolog
% Counting On requires stable order principle
pp_necessity(counting_on, stable_order_principle).

% Can deploy if prerequisites met
incompatibility_semantics:material_inference(
    [s(possesses(Agent, iterated_succession)),
     s(possesses(Agent, termination_condition))],
    s(exp_poss(deploys(Agent, counting_on))),
    true
).
```

### Pathologies Are Detectable

Bad Infinites and incoherence in mathematical content:

```prolog
% Being ↔ Nothing cycle (Hegelian Bad Infinite)
proves([s(empty_set)] => [s(comp_nec(being))], ...).
proves([s(being)] => [s(comp_nec(nothing))], ...).
% Cycle detected by critique.pl

% Russell's Paradox
incoherent([s(comp_nec(set_of_all_sets))]).
% Incoherence detected automatically
```

## How It Works

1. **Load the framework**: `swipl load.pl`
2. **Mathematical content** (metaphors, strategies) becomes material inferences
3. **The prover** uses these inferences during proof search
4. **The critique module** detects pathologies (cycles, incoherence, missing prerequisites)
5. **The ORR cycle** attempts accommodation when failures occur

## Examples

### Example 1: Grounding Metaphor

```prolog
?- proves([s(collection([a,b,c])), s(size([a,b,c], 3))] => [s(number(3))], 50, _, Proof).
% Proof uses "Arithmetic Is Object Collection" metaphor
```

### Example 2: Strategy Deployment

```prolog
?- proves([s(problem(addition(5, 3)))] => [s(comp_nec(sequence([6, 7, 8])))], 50, _, Proof).
% Proof uses "Counting On" strategy
```

### Example 3: Pathology Detection

```prolog
?- incoherent([s(comp_nec(set_of_all_sets))]).
% Output: PATHOLOGY: Russell's Paradox - set of all sets is incoherent
true.
```

## Architecture

```
PML Core Framework
├── Core Logic
│   ├── incompatibility_semantics.pl (prover)
│   ├── semantic_axioms.pl (U→A→LG→U')
│   └── critique.pl (ORR cycle)
│
└── Content Modules ⭐ NEW
    ├── lakoff_metaphors.pl (embodied cognition)
    └── arithmetic_strategies.pl (Brandomian practices)
```

## Testing

```bash
# All integration tests
swipl -g run_all_tests -t halt tests/lakoff_brandom_test.pl

# Original core tests (ensure compatibility)
swipl -g run_tests -t halt tests/simple_test.pl

# Critique mechanism tests
swipl -g run_all_tests -t halt tests/critique_test.pl
```

## Documentation

- **[LAKOFF_BRANDOM_INTEGRATION.md](tests/LAKOFF_BRANDOM_INTEGRATION.md)**: Complete technical documentation
- **[TEST_SUMMARY.md](tests/TEST_SUMMARY.md)**: Original core framework tests
- **[CRITIQUE_IMPLEMENTATION.md](tests/CRITIQUE_IMPLEMENTATION.md)**: Critique mechanism details

## Status

✅ **COMPLETE AND TESTED**
- 29/29 integration tests passing
- 10/10 original core tests passing
- 7/7 critique mechanism tests passing
- 0 regressions introduced

## For the Book

This implementation provides concrete supplementary material demonstrating:

1. **Embodied cognition is formalizable** (Lakoff's metaphors → executable logic)
2. **Pragmatism is computational** (Brandom's practices → running programs)
3. **Dialectical logic has content** (Hegel's patterns in real mathematics)

The system is not just *abstract modal logic*—it's *logic about actual mathematical practices* informed by cognitive science and pragmatist philosophy.

## Next Steps

The math/ folder contains 20+ additional arithmetic strategy files (sar_*.pl, smr_*.pl) that can be progressively integrated using the patterns established here. Each file represents a specific reasoning strategy (Single-Addend Reasoning, Single-Multiplier Reasoning, etc.) that can be analyzed for PP-necessities/sufficiencies and added to the content repository.

---

**This is embodied logic in action.**
