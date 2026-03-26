# Meaning-Use Analysis Report

Generated: 2025-10-12 10:27:40

================================================================================

## Overview

**Strategies Analyzed:** 21
**Computational Patterns Identified:** 2
**Elaboration Relationships Discovered:** 16

### Strategies
- ADD_COBO (1 patterns)
- ADD_Chunking (1 patterns)
- ADD_Counting (1 patterns)
- ADD_RMB (0 patterns)
- ADD_Rounding (1 patterns)
- CBBO (0 patterns)
- COBO (1 patterns)
- ChunkingA (0 patterns)
- ChunkingB (0 patterns)
- ChunkingC (0 patterns)
- DIV_CGOB (1 patterns)
- DIV_DealingByOnes (1 patterns)
- DIV_IDR (0 patterns)
- DIV_UCR (0 patterns)
- MULT_C2C (1 patterns)
- MULT_CBO (0 patterns)
- MULT_Commutative_Reasoning (0 patterns)
- MULT_DR (0 patterns)
- SUB_Decomposition (0 patterns)
- SUB_Rounding (1 patterns)
- SUB_Sliding (0 patterns)

## Theoretical Framework

This report analyzes student arithmetic strategies using Robert Brandom's Meaning-Use Analysis
(MUA) framework from "Between Saying and Doing: Towards an Analytic Pragmatism."

### Key Concepts

**1. Practice-Vocabulary Relations:**
- **PV-Sufficiency**: Practices (P) are sufficient to deploy a Vocabulary (V)
  - What you must DO to correctly use the vocabulary
- **VP-Sufficiency**: Vocabulary (V) is sufficient to specify Practices (P)
  - What you can SAY to describe what needs to be done

**2. Practice-Practice Relations (PP-Sufficiency):**
- **Algorithmic Elaboration**: Complex practice decomposable into primitive practices + algorithm
  - Example: Long division = repeated subtraction + place value tracking
  - Fully mechanizable; no creative insight needed

- **Practical Elaboration through Training** (Pragmatic Projection):
  - New practice developed from prior practice through experience
  - Cannot be fully algorithmically decomposed
  - Requires "going on in the same way" (Wittgenstein)
  - Involves genuine learning, not just mechanical execution

**3. Pragmatic Metavocabulary:**
- V₁ is a pragmatic metavocabulary for V₂ when:
  - V₁ can specify the practices P₂ needed to deploy V₂
  - Often V₁ is expressively weaker than V₂

**4. LX Relation (Elaborated-Explicating):**
- V' is LX to V when V' is:
  - **(L) Elaborated from** the practices underlying V
  - **(X) Explicating of** those practices (makes implicit explicit)
- LX captures conceptual progress: knowing-how → knowing-that

**5. Pragmatic Expressive Bootstrapping:**
- A weaker vocabulary can serve as metavocabulary for a stronger one
- Example: Non-indexical language can specify use of indexical language
- Explains how embodied practices ground abstract mathematics

### What This Analysis Can Determine

Based on AST analysis of Python automaton implementations, this system identifies:

✅ **Computational patterns** as proxies for primitive practices
✅ **Algorithmic elaborations** where strategies share computational structure
✅ **PP-sufficiency chains** showing prerequisite relationships
✅ **Candidate LX relations** where complex strategies make simple ones explicit

⚠️ **Limitations**:
- Cannot determine semantic content or conceptual metaphors from code alone
- Cannot identify practical elaborations requiring genuine insight
- Patterns are computational, not necessarily cognitive
- LX relations are candidates requiring philosophical verification


## Computational Patterns as Primitive Practices

The following patterns represent primitive computational practices
identified in the strategy implementations:

### Pattern: `base_decomposition`

**Type:** decomposition
**Used by:** 4 strategies
**Strategies:** ADD_Rounding, DIV_CGOB, DIV_DealingByOnes, SUB_Rounding

**Interpretation as Practice (P_base_decomposition):**
- Breaking numbers into base-10 components (tens, ones)
- Computational signature: `num // 10`, `num % 10`
- Cognitive analogue: Place value understanding
- **PP-Necessity for:** Most multi-digit strategies

### Pattern: `incremental_counting`

**Type:** counting
**Used by:** 5 strategies
**Strategies:** ADD_COBO, ADD_Chunking, ADD_Counting, COBO, MULT_C2C

**Interpretation as Practice (P_incremental_counting):**
- State-based iteration with accumulation
- Computational signature: `while` loops, register increments
- Cognitive analogue: Iterated succession, counting on
- **PP-Necessity for:** Strategies building on counting

## Conceptual Metaphor Analysis (Lakoff & Núñez)

This section analyzes which strategies share embodied conceptual metaphors,
revealing foundational grounding metaphors vs. specialized ones.

### "Arithmetic as Object Manipulation"

**Used by 3 strategies:**
ADD_Chunking, CBBO, SUB_Decomposition

**Source Domain:** Object Manipulation
**Target Domain:** Arithmetic
**Key Entailments:** A collection can be augmented by adding other collections to it. It's often easier to add organized groups (like ten-blocks) first, then smaller items.

**Interpretation:** This is a **common metaphor** used across multiple operations.
Strategies using this metaphor form a conceptual cluster.

---

### "Arithmetic as Motion Along a Path"

**Used by 3 strategies:**
ADD_COBO, COBO, SUB_Sliding

**Source Domain:** Motion
**Target Domain:** Arithmetic
**Key Entailments:** Moving along a path can be done in segments. The final position is the sum of the starting position and the lengths of all segments.

**Interpretation:** This is a **common metaphor** used across multiple operations.
Strategies using this metaphor form a conceptual cluster.

---

### "Arithmetic as Object Collection"

**Used by 2 strategies:**
MULT_CBO, MULT_Commutative_Reasoning

**Source Domain:** Object Collection
**Target Domain:** Arithmetic
**Key Entailments:** A collection of groups can be counted by first counting the items in the large groups (bases) and then counting the items in the small groups (ones).

**Interpretation:** This is a **specialized metaphor** for specific strategies.
May represent advanced or operation-specific conceptualizations.

---

### "Numbers as Physical Objects"

**Used by 1 strategy:**
ADD_RMB

**Source Domain:** Object Collection
**Target Domain:** Arithmetic
**Key Entailments:** A collection can be split and its parts moved without changing the total quantity. Rearranging parts makes counting easier.

**Interpretation:** This is a **specialized metaphor** for specific strategies.
May represent advanced or operation-specific conceptualizations.

---

### Summary

**Total unique metaphors identified:** 4
- **Foundational (≥5 strategies):** 0
- **Common (3-4 strategies):** 2
  - Arithmetic as Object Manipulation, Arithmetic as Motion Along a Path
- **Specialized (<3 strategies):** 2
  - Arithmetic as Object Collection, Numbers as Physical Objects

**Pedagogical Implication:** Students who struggle with foundational metaphors
may need remediation in the corresponding embodied source domains before
advancing to abstract arithmetic strategies.

## PP-Sufficiency: Algorithmic Elaborations

These are **algorithmic elaborations** where one strategy's practices
are computationally sufficient for another strategy.

### Elaborations via: `base_decomposition`

**ADD_Rounding → SUB_Rounding** (confidence: 1.00)
- **Type:** Algorithmic Elaboration
- **PP-Sufficiency:** P_ADD_Rounding is sufficient for P_SUB_Rounding
- **Shared practices:** base_decomposition
- **Interpretation:** SUB_Rounding builds on computational patterns from ADD_Rounding

**ADD_Rounding → DIV_CGOB** (confidence: 1.00)
- **Type:** Algorithmic Elaboration
- **PP-Sufficiency:** P_ADD_Rounding is sufficient for P_DIV_CGOB
- **Shared practices:** base_decomposition
- **Interpretation:** DIV_CGOB builds on computational patterns from ADD_Rounding

**ADD_Rounding → DIV_DealingByOnes** (confidence: 1.00)
- **Type:** Algorithmic Elaboration
- **PP-Sufficiency:** P_ADD_Rounding is sufficient for P_DIV_DealingByOnes
- **Shared practices:** base_decomposition
- **Interpretation:** DIV_DealingByOnes builds on computational patterns from ADD_Rounding

**SUB_Rounding → DIV_CGOB** (confidence: 1.00)
- **Type:** Algorithmic Elaboration
- **PP-Sufficiency:** P_SUB_Rounding is sufficient for P_DIV_CGOB
- **Shared practices:** base_decomposition
- **Interpretation:** DIV_CGOB builds on computational patterns from SUB_Rounding

**SUB_Rounding → DIV_DealingByOnes** (confidence: 1.00)
- **Type:** Algorithmic Elaboration
- **PP-Sufficiency:** P_SUB_Rounding is sufficient for P_DIV_DealingByOnes
- **Shared practices:** base_decomposition
- **Interpretation:** DIV_DealingByOnes builds on computational patterns from SUB_Rounding

**DIV_CGOB → DIV_DealingByOnes** (confidence: 1.00)
- **Type:** Algorithmic Elaboration
- **PP-Sufficiency:** P_DIV_CGOB is sufficient for P_DIV_DealingByOnes
- **Shared practices:** base_decomposition
- **Interpretation:** DIV_DealingByOnes builds on computational patterns from DIV_CGOB

### Elaborations via: `incremental_counting`

**ADD_Counting → ADD_Chunking** (confidence: 1.00)
- **Type:** Algorithmic Elaboration
- **PP-Sufficiency:** P_ADD_Counting is sufficient for P_ADD_Chunking
- **Shared practices:** incremental_counting
- **Interpretation:** ADD_Chunking builds on computational patterns from ADD_Counting

**ADD_Counting → ADD_COBO** (confidence: 1.00)
- **Type:** Algorithmic Elaboration
- **PP-Sufficiency:** P_ADD_Counting is sufficient for P_ADD_COBO
- **Shared practices:** incremental_counting
- **Interpretation:** ADD_COBO builds on computational patterns from ADD_Counting

**ADD_Counting → COBO** (confidence: 1.00)
- **Type:** Algorithmic Elaboration
- **PP-Sufficiency:** P_ADD_Counting is sufficient for P_COBO
- **Shared practices:** incremental_counting
- **Interpretation:** COBO builds on computational patterns from ADD_Counting

**ADD_Counting → MULT_C2C** (confidence: 1.00)
- **Type:** Algorithmic Elaboration
- **PP-Sufficiency:** P_ADD_Counting is sufficient for P_MULT_C2C
- **Shared practices:** incremental_counting
- **Interpretation:** MULT_C2C builds on computational patterns from ADD_Counting

**ADD_Chunking → ADD_COBO** (confidence: 1.00)
- **Type:** Algorithmic Elaboration
- **PP-Sufficiency:** P_ADD_Chunking is sufficient for P_ADD_COBO
- **Shared practices:** incremental_counting
- **Interpretation:** ADD_COBO builds on computational patterns from ADD_Chunking

**ADD_Chunking → COBO** (confidence: 1.00)
- **Type:** Algorithmic Elaboration
- **PP-Sufficiency:** P_ADD_Chunking is sufficient for P_COBO
- **Shared practices:** incremental_counting
- **Interpretation:** COBO builds on computational patterns from ADD_Chunking

**ADD_Chunking → MULT_C2C** (confidence: 1.00)
- **Type:** Algorithmic Elaboration
- **PP-Sufficiency:** P_ADD_Chunking is sufficient for P_MULT_C2C
- **Shared practices:** incremental_counting
- **Interpretation:** MULT_C2C builds on computational patterns from ADD_Chunking

**ADD_COBO → COBO** (confidence: 1.00)
- **Type:** Algorithmic Elaboration
- **PP-Sufficiency:** P_ADD_COBO is sufficient for P_COBO
- **Shared practices:** incremental_counting
- **Interpretation:** COBO builds on computational patterns from ADD_COBO

**ADD_COBO → MULT_C2C** (confidence: 1.00)
- **Type:** Algorithmic Elaboration
- **PP-Sufficiency:** P_ADD_COBO is sufficient for P_MULT_C2C
- **Shared practices:** incremental_counting
- **Interpretation:** MULT_C2C builds on computational patterns from ADD_COBO

**COBO → MULT_C2C** (confidence: 1.00)
- **Type:** Algorithmic Elaboration
- **PP-Sufficiency:** P_COBO is sufficient for P_MULT_C2C
- **Shared practices:** incremental_counting
- **Interpretation:** MULT_C2C builds on computational patterns from COBO


**Note on Algorithmic vs. Practical Elaboration:**
- These are *algorithmic* elaborations (code reuse, shared subroutines)
- *Practical* elaborations (insight, training) cannot be detected from code
- Cognitive development may involve practical elaboration not visible here
## LX Relations: Candidate Elaborated-Explicating Pairs

Candidate LX relations where a complex strategy may make
a simpler strategy's implicit practices explicit.

### Candidate: `ADD_Chunking` as LX

**Elaborated from:** ADD_Counting
**Explicates for:** ADD_COBO, COBO, MULT_C2C

**Why this might be LX:**
- ADD_Chunking builds on practices from ADD_Counting
- ADD_Chunking provides structure used by ADD_COBO, COBO, MULT_C2C
- May make implicit patterns in ADD_Counting explicit

**Verification needed:** Philosophical analysis of whether ADD_Chunking
genuinely explicates (makes sayable) what ADD_Counting only does.

### Candidate: `ADD_COBO` as LX

**Elaborated from:** ADD_Counting, ADD_Chunking
**Explicates for:** COBO, MULT_C2C

**Why this might be LX:**
- ADD_COBO builds on practices from ADD_Counting, ADD_Chunking
- ADD_COBO provides structure used by COBO, MULT_C2C
- May make implicit patterns in ADD_Counting explicit

**Verification needed:** Philosophical analysis of whether ADD_COBO
genuinely explicates (makes sayable) what ADD_Counting only does.

### Candidate: `COBO` as LX

**Elaborated from:** ADD_Counting, ADD_Chunking, ADD_COBO
**Explicates for:** MULT_C2C

**Why this might be LX:**
- COBO builds on practices from ADD_Counting, ADD_Chunking, ADD_COBO
- COBO provides structure used by MULT_C2C
- May make implicit patterns in ADD_Counting explicit

**Verification needed:** Philosophical analysis of whether COBO
genuinely explicates (makes sayable) what ADD_Counting only does.

### Candidate: `SUB_Rounding` as LX

**Elaborated from:** ADD_Rounding
**Explicates for:** DIV_CGOB, DIV_DealingByOnes

**Why this might be LX:**
- SUB_Rounding builds on practices from ADD_Rounding
- SUB_Rounding provides structure used by DIV_CGOB, DIV_DealingByOnes
- May make implicit patterns in ADD_Rounding explicit

**Verification needed:** Philosophical analysis of whether SUB_Rounding
genuinely explicates (makes sayable) what ADD_Rounding only does.

### Candidate: `DIV_CGOB` as LX

**Elaborated from:** ADD_Rounding, SUB_Rounding
**Explicates for:** DIV_DealingByOnes

**Why this might be LX:**
- DIV_CGOB builds on practices from ADD_Rounding, SUB_Rounding
- DIV_CGOB provides structure used by DIV_DealingByOnes
- May make implicit patterns in ADD_Rounding explicit

**Verification needed:** Philosophical analysis of whether DIV_CGOB
genuinely explicates (makes sayable) what ADD_Rounding only does.
