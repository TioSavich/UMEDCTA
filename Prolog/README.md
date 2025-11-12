# PML Core Framework

**Polarized Modal Logic** - A domain-agnostic framework for embodied, pragmatic, dialectical reasoning.

## Overview

The PML Core Framework implements:
- **Polarized Modal Logic**: 3 contexts (S/O/N) × 4 modalities (comp_nec, exp_nec, comp_poss, exp_poss)
- **Incompatibility Semantics**: Brandomian material inferences with commitment/entitlement tracking
- **Resource-Tracked Proving**: Cognitive budget constraints on proof search
- **Dialectical Dynamics**: U → A → LG → U' rhythm with compressive/expansive transitions
- **Critique Mechanisms**: ORR cycle (Observe → Reflect → Reorganize → Retry)
- **Trace Mechanisms**: Möbius dynamic for proof erasure and resistance to stabilization

## Structure

```
Prolog/
├── load.pl                         # Master loader
├── Core Modules/
│   ├── pml_operators.pl            # Modal operators and vocabulary
│   ├── utils.pl                    # Helper predicates
│   ├── incompatibility_semantics.pl # Embodied prover (11K lines)
│   ├── automata.pl                 # Highlander, Arche-Trace, Primes
│   ├── semantic_axioms.pl          # PML dynamics (U→A→LG→U')
│   ├── pragmatic_axioms.pl         # I_f, Unsatisfiable Desire
│   ├── intersubjective_praxis.pl   # Oobleck, Recognition
│   ├── critique.pl                 # Pathology detection, accommodation
│   └── dialectical_engine.pl       # FSM execution, ORR cycle
│
├── tests/                          # Core framework tests
│   ├── simple_test.pl              # 10 basic tests
│   ├── core_test.pl                # Comprehensive test suite
│   ├── critique_test.pl            # 7 critique mechanism tests
│   ├── TEST_SUMMARY.md             # Test documentation
│   └── CRITIQUE_IMPLEMENTATION.md  # Critique technical docs
│
└── math/                           # Math domain instantiation
    ├── README.md                   # Math domain documentation
    ├── load_math.pl                # Math content loader
    ├── lakoff_metaphors.pl         # Embodied metaphors (Lakoff)
    ├── arithmetic_strategies.pl    # Brandomian strategies
    ├── lakoff_brandom_test.pl      # 29 math content tests
    └── *.pl                        # Legacy strategy files
```

## Quick Start

### Load Core Framework Only

```bash
swipl load.pl
```

### Run Core Tests

```bash
cd tests
swipl -g run_tests -t halt simple_test.pl      # 10 basic tests
swipl -g run_all_tests -t halt core_test.pl    # Comprehensive
swipl -g run_all_tests -t halt critique_test.pl # Critique mechanisms
```

### Load with Math Domain

```bash
cd math
swipl load_math.pl

# Run math tests
swipl -g run_all_tests -t halt lakoff_brandom_test.pl  # 29 tests
```

## Core Concepts

### 1. Polarized Modal Logic (PML)

Three **modal contexts**:
- **S** (Subjective): Felt experience, embodied practices
- **O** (Objective): Stabilized, reified objects
- **N** (Normative): Ought-to-be, social practices

Four **modalities** per context:
- **comp_nec** (□↓): Compressive necessity (tension, constraint)
- **exp_nec** (□↑): Expansive necessity (release, must-become)
- **comp_poss** (◊↓): Compressive possibility (can-tighten)
- **exp_poss** (◊↑): Expansive possibility (can-expand)

### 2. Dialectical Rhythm

The fundamental U → A → LG → U' pattern:
- **U** (Undifferentiated): Pre-reflective unity
- **A** (Awareness): Compressive tension, negation
- **LG** (Logical Genesis): Expansive resolution
- **U'** (Unity Prime): Enriched unity

Implemented as:
```prolog
material_inference([s(u)], s(comp_nec(a)), true).
material_inference([s(a)], s(exp_poss(lg)), true).
material_inference([s(lg)], s(exp_nec(u_prime)), true).
```

### 3. The Arche-Trace (Möbius Dynamic)

Using SWI-Prolog attributed variables to model the **elusive subject** (I_f):
- Resists stabilization (unification with concrete terms fails)
- Contaminates proofs (trace propagation → proof erasure)
- Implements the "Unsatisfiable Desire" (C_Id cannot represent I_f)

### 4. Critique Mechanisms

**Detection**:
- Bad Infinites (cycle detection in proof trees)
- Incoherence (P ∧ ¬P detection)
- Resource exhaustion (budget depletion)

**Accommodation**:
- Stress map tracking (which commitments fail most)
- Belief revision (dynamic assertion of incoherence)
- Sublation diagnostics (signals need for higher concepts)

**ORR Cycle**:
```prolog
run_computation(Sequent, Limit) :-
    catch(
        proves(Sequent, Limit, _, Proof),
        perturbation(Type),
        handle_perturbation(Type, Sequent, Limit)
    ).

handle_perturbation(Error, Sequent, Limit) :-
    accommodate(Error) ->
        run_computation(Sequent, Limit)  % Retry
    ;
        fail.  % Halt
```

## Test Results

### Core Framework Tests

**10/10 Simple Tests** ✅
- Module loading
- Automata (Highlander, Primes, Trace)
- Prover basics (Identity, Explosion)
- PML dynamics (Dialectical rhythm, Oobleck)
- Pragmatic axioms (I-Feeling, Unsatisfiable Desire)

**7/7 Critique Tests** ✅
- Stress map tracking
- Commitment extraction
- Bad Infinite detection (cycle finding)
- Stressed commitment identification
- Resource exhaustion handling
- Incoherence accommodation
- Sublation mechanism

### Math Domain Tests

**29/29 Math Content Tests** ✅
- Grounding metaphors (The 4Gs)
- Basic Metaphor of Infinity (BMI)
- BMI pathologies (Being↔Nothing, Zeno, Russell)
- Arithmetic strategies (Sliding, Counting On, RMB)
- PP-necessities and sufficiencies
- LX-relations (elaboration)
- PML dynamics integration
- ORR cycle with strategies
- Conceptual blends (Euler's formula)

## Key Features

### 1. Domain-Agnostic Core

The core framework makes **no assumptions** about domain content. It provides:
- Logic (modal operators, inference rules)
- Mechanisms (proof search, critique, trace)
- Architecture (ORR cycle, dialectical rhythm)

Domain instantiations (like math/) add:
- Material inferences (domain-specific "axioms")
- Practices (strategies, heuristics, patterns)
- Content for critique (pathologies, incoherences)

### 2. Embodied Reasoning

**Not** abstract symbol manipulation. The prover:
- Tracks **modal context** (S/O/N) and switches have **cost**
- Consumes **cognitive resources** (budget depletion → failure)
- Exhibits **modal dynamics** (compression → tension, expansion → release)
- Can **fail** (resource exhaustion, incoherence, cycles)

### 3. Self-Critique

The system can detect its own limitations:
- **Bad Infinites**: Closed compressive cycles (e.g., Being ↔ Nothing)
- **Incoherence**: Contradictory commitments
- **Resource Limits**: Cannot prove within budget
- **Missing Prerequisites**: Practices lack foundations

This is **not** just error handling—it's **reflection** on the system's own structure.

### 4. Proof Erasure

Proofs involving trace (I_f) are **erased**, not just marked invalid:
```prolog
construct_proof(Rule, Sequent, SubProofs, Proof) :-
    ( member(erasure(_), SubProofs) ->
        Proof = erasure(propagation)  % Contamination
    ; contains_trace(Sequent) ->
        Proof = erasure(Rule)          % Entity-level
    ;
        Proof = proof(Rule, Sequent, SubProofs)  % Normal
    ).
```

This models the **impossibility of objectifying the subject**.

## Theoretical Foundations

### Brandom's Inferentialism

Meaning is **use** in material-inferential practices:
- Material inferences (not formal deduction)
- Commitment and entitlement tracking
- Social-normative pragmatics

### Lakoff's Embodied Cognition

Mathematical concepts are **grounded** in sensory-motor experience:
- Grounding metaphors (4Gs: Collection, Construction, Measurement, Motion)
- Linking metaphors (extending to abstract domains)
- Conceptual blends (integrating multiple metaphors)

### Hegel's Dialectical Logic

**Not** thesis-antithesis-synthesis, but:
- Determinate negation (specific tension, not general "not")
- Bad Infinite vs. True Infinite (closed vs. open spirals)
- Sublation (Aufhebung): preserving-while-transcending

## For Developers

### Adding Domain Content

1. Create a domain folder: `Prolog/[domain]/`
2. Create a loader: `load_[domain].pl` that loads `../load.pl` then your modules
3. Define material inferences for your domain
4. Define practices (strategies, heuristics) with prerequisites
5. Write tests showing critique mechanisms work

Example structure:
```
Prolog/physics/
├── load_physics.pl
├── newtonian_mechanics.pl
├── lagrangian_mechanics.pl
└── physics_test.pl
```

### Module Architecture

All modules use **multifile predicates** to extend the core prover:
```prolog
:- multifile incompatibility_semantics:material_inference/3.
:- multifile incompatibility_semantics:is_incoherent/1.

incompatibility_semantics:material_inference(
    [Antecedents],
    Consequent,
    Body  % Callable predicate (often just 'true')
).
```

### Testing Pattern

```prolog
run_test(Name, Goal) :-
    format('~n[TEST] ~w~n', [Name]),
    ( catch(Goal, Error, (format('  ERROR: ~w~n', [Error]), fail)) ->
        writeln('  PASS')
    ;
        writeln('  FAIL')
    ).
```

## Documentation

- **[tests/TEST_SUMMARY.md](tests/TEST_SUMMARY.md)**: Core framework test results
- **[tests/CRITIQUE_IMPLEMENTATION.md](tests/CRITIQUE_IMPLEMENTATION.md)**: Critique mechanisms technical documentation
- **[math/README.md](math/README.md)**: Math domain documentation
- **[math/LAKOFF_BRANDOM_INTEGRATION.md](math/LAKOFF_BRANDOM_INTEGRATION.md)**: Math integration technical details

## Status

✅ **PRODUCTION READY**

- **Core Framework**: Complete and tested (10/10 + 7/7 tests passing)
- **Math Domain**: Complete and tested (29/29 tests passing)
- **Documentation**: Extensive
- **Domain Separation**: Clean (core is agnostic, math is in math/)

## For the Book

This implementation provides supplementary materials demonstrating:

1. **Embodied cognition is formalizable**: Lakoff's metaphors become executable logic
2. **Pragmatism is computational**: Brandom's inferentialism becomes running programs
3. **Dialectical logic is not abstract**: Hegel's patterns appear in real content

The system is **not** just modal logic—it's **logic that reasons about practices** using insights from cognitive science, pragmatist philosophy, and dialectical thinking.

---

**This is what it means for logic to be embodied, pragmatic, and dialectical.**
