# Math Domain Content for PML Framework

This directory contains mathematical content that can be reasoned about using the PML (Polarized Modal Logic) Core Framework.

## Overview

The PML Core Framework (in the parent directory) is **domain-agnostic**. This math/ folder demonstrates how to instantiate it with specific domain content—in this case, mathematical reasoning patterns from cognitive science and pragmatist philosophy.

## Structure

```
math/
├── load_math.pl                    # Loads core + math content
├── lakoff_metaphors.pl             # Embodied mathematical metaphors
├── arithmetic_strategies.pl        # Brandomian strategy analysis
├── lakoff_brandom_test.pl          # Comprehensive test suite (29 tests)
├── LAKOFF_BRANDOM_INTEGRATION.md   # Technical documentation
├── LAKOFF_BRANDOM_README.md        # User guide
└── *.pl                            # Legacy arithmetic strategy files (to be integrated)
```

## Quick Start

```bash
# Load math content
cd math
swipl load_math.pl

# Run tests
swipl -g run_all_tests -t halt lakoff_brandom_test.pl
```

## What's Included

### 1. Lakoff's Embodied Metaphors ([lakoff_metaphors.pl](lakoff_metaphors.pl))

**Source**: Lakoff & Núñez, "Where Mathematics Comes From: How the Embodied Mind Brings Mathematics into Being"

Represents conceptual metaphors as material inferences:

- **The 4Gs** (Grounding Metaphors): Object Collection, Object Construction, Measuring Stick, Motion Along Path
- **The BMI** (Basic Metaphor of Infinity): How humans conceptualize actual infinity
- **Pathologies**: Being↔Nothing, Zeno's Paradox, Russell's Paradox
- **Blends**: Euler's formula (e^(iπ) + 1 = 0)

### 2. Brandomian Strategies ([arithmetic_strategies.pl](arithmetic_strategies.pl))

**Source**: Brandom's inferentialist semantics applied to arithmetic reasoning

Represents strategies as practices with normative structure:

- **Sliding**: Difference invariance (18 - 9 = 19 - 10)
- **Counting On**: Addition as rhythmic succession (5 + 3 → "6, 7, 8")
- **Rearranging to Make Bases**: Strategic decomposition (28 + 7 = 30 + 5)

Each strategy has:
- PP-necessities (prerequisite practices)
- PP-sufficiencies (deployment conditions)
- LX-relations (elaboration hierarchies)

### 3. Legacy Strategy Files (*.pl)

~20 arithmetic strategy files (sar_*.pl, smr_*.pl, etc.) representing specific reasoning patterns. These are **not yet integrated** but follow similar patterns and can be progressively incorporated.

## Example Usage

### Using a Grounding Metaphor

```prolog
?- ['load_math.pl'].
?- proves([s(collection([a,b,c])), s(size([a,b,c], 3))] => [s(number(3))], 50, _, Proof).
% Uses "Arithmetic Is Object Collection" metaphor
```

### Deploying a Strategy

```prolog
?- proves([s(problem(addition(5, 3)))] => [s(comp_nec(sequence([6, 7, 8])))], 50, _, Proof).
% Uses "Counting On" strategy
```

### Detecting Pathologies

```prolog
?- incoherent([s(comp_nec(set_of_all_sets))]).
% Output: PATHOLOGY: Russell's Paradox
true.
```

## Test Results

**29/29 tests passing** ✅

- Grounding metaphors: 3/3
- BMI metaphors: 3/3
- BMI pathologies: 3/3
- Arithmetic strategies: 4/4
- PP-necessities: 4/4
- LX-relations: 3/3
- PML dynamics: 4/4
- ORR cycle: 3/3
- Conceptual blends: 2/2

## Integration Path for Legacy Files

To integrate existing .pl files (sar_*.pl, smr_*.pl, etc.):

1. **Identify the strategy**: What reasoning pattern does it implement?
2. **Extract the inference**: What is the core material inference?
3. **Identify prerequisites**: What practices must the agent possess?
4. **Add to arithmetic_strategies.pl**: Follow the pattern of existing strategies
5. **Write tests**: Verify deployment and critique work correctly

## Documentation

- **[LAKOFF_BRANDOM_INTEGRATION.md](LAKOFF_BRANDOM_INTEGRATION.md)**: Full technical documentation (architecture, design decisions, test results)
- **[LAKOFF_BRANDOM_README.md](LAKOFF_BRANDOM_README.md)**: User-oriented guide with examples

## Design Philosophy

### Math Content as Critique-able Material

The math modules aren't just "examples"—they're **content the system reasons about and critiques**:

- **Bad Infinites** in conceptual metaphors are detected by cycle analysis
- **Incoherence** in set-theoretic constructions triggers accommodation
- **Prerequisite violations** in strategy deployment create failure modes
- **LX-relations** provide elaboration hierarchies for conceptual development

### Modal Structure of Mathematical Practice

Mathematical reasoning isn't just symbol manipulation—it has **modal dynamics**:

- **Compression** (S → comp_nec): Strategies simplify problems under tension
- **Expansion** (S → exp_nec): Solutions release tension
- **Tension** (S → comp_nec → A): Failures create awareness of inadequacy
- **ORR Cycle**: Observe → Reflect → Reorganize → Retry

This aligns with the dialectical rhythm U → A → LG → U'.

## Relationship to Core Framework

The core PML framework (parent directory) provides:
- Modal logic (S/O/N contexts, 4 modalities)
- Resource-tracked theorem proving
- Incompatibility semantics
- Critique mechanisms (ORR cycle, stress tracking)
- Trace mechanisms (Arche-Trace, proof erasure)

The math modules provide:
- **Content** (metaphors, strategies) as material inferences
- **Test cases** for critique mechanisms
- **Demonstrations** of how domain knowledge integrates

## Status

✅ **COMPLETE AND TESTED**
- Core integration: Complete
- Test coverage: Comprehensive
- Documentation: Extensive
- Domain separation: Clean

## For Other Domains

This math/ folder serves as a **template** for domain-specific instantiations:

```
Prolog/                      # Core framework (domain-agnostic)
  ├── load.pl
  ├── incompatibility_semantics.pl
  ├── critique.pl
  └── ...

Prolog/math/                 # Math domain
  ├── load_math.pl
  ├── lakoff_metaphors.pl
  └── arithmetic_strategies.pl

Prolog/[your_domain]/        # Your domain
  ├── load_[domain].pl
  ├── domain_content.pl
  └── domain_strategies.pl
```

The pattern is:
1. Load core framework
2. Add domain-specific material inferences
3. Define domain-specific practices (strategies, heuristics, patterns)
4. Write tests demonstrating critique mechanisms work on your content

---

**This is embodied, pragmatic, dialectical logic applied to mathematics.**
