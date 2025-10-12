# UMEDCTA Supplementary Code Documentation

This document provides a comprehensive guide to the computational implementations supporting *Understanding Mathematics as an Emancipatory Discipline: A Critical Theory Approach*.

**All code is accessible at:** https://tiosavich.github.io/UMEDCTA/

---

## Table of Contents

1. [The Hermeneutic Calculator (Prolog Implementation)](#1-the-hermeneutic-calculator-prolog-implementation)
2. [LK_RB_Synthesis: Algorithmic Elaboration Discovery](#2-lk_rb_synthesis-algorithmic-elaboration-discovery)
3. [Interactive Web Interfaces](#3-interactive-web-interfaces)
4. [Philosophical Teaching Modules](#4-philosophical-teaching-modules)
5. [How to Cite These Materials](#5-how-to-cite-these-materials)

---

## 1. The Hermeneutic Calculator (Prolog Implementation)

**URL:** https://tiosavich.github.io/UMEDCTA/Calculator/Prolog/

**Full README:** https://tiosavich.github.io/UMEDCTA/Calculator/Prolog/readme.md

### What It Does

The Hermeneutic Calculator (HC) is a formal system implemented in SWI-Prolog that models children's arithmetic strategies as computational automata. It serves three primary functions:

1. **Formalizes Student-Invented Strategies**: Implements 17+ strategies from CGI (Cognitively Guided Instruction) research, preserving the cognitive phenomenology of how students actually solve problems
2. **Implements Brandomian Incompatibility Semantics**: The first computational implementation of Robert Brandom's logic of material inference
3. **Models Crisis-Driven Learning**: Implements a computational version of Piagetian equilibration and Hegelian determinate negation through the Observe-Reorganize-Reflect (ORR) cycle

### Core Architecture

#### FSM Engine Architecture
**Files:** `fsm_engine.pl`, `grounded_arithmetic.pl`, `grounded_utils.pl`

A unified finite state machine engine that standardizes all student strategy execution, providing:
- Consistent modal logic integration (`s/1`, `comp_nec/1`, `exp_poss/1` operators)
- Cognitive cost tracking for every operation
- Grounded arithmetic foundation (numbers as recollection structures, not abstract objects)

**Theoretical Significance:** The FSM engine demonstrates that informal student thinking has rigorous formal structure. The modal operators connect computational steps to Brandomian incompatibility semantics.

#### The ORR Cycle (Observe-Reorganize-Reflect)
**Files:** `execution_handler.pl`, `meta_interpreter.pl`, `reflective_monitor.pl`, `reorganization_engine.pl`

The system's learning capability, modeling Piagetian cognitive development:
- **Observe**: Meta-interpreter produces execution traces, making reasoning observable to itself
- **Reflect**: Analyzes traces for "disequilibrium" (goal failures, contradictions)
- **Reorganize**: Modifies its own knowledge base to resolve conflicts

**Theoretical Significance:** This is a computational model of determinate negation—the system recognizes its own limits and transcends them through self-modification.

#### Incompatibility Semantics
**File:** `incompatibility_semantics.pl`

Implements Brandom's logic where meaning is defined by material incompatibility rather than truth tables. For example, "square" is incompatible with "circular"—this incompatibility *constitutes* the meaning of "square."

**Theoretical Significance:** Formalizes the claim that mathematical concepts are defined by what they rule out, not by reference to abstract objects.

#### Student Strategy Models
**Files:** `sar_*.pl` (addition/subtraction), `smr_*.pl` (multiplication/division)

17+ models of actual student strategies, all unified under the FSM engine. Examples:
- `sar_add_cobo.pl`: Counting On by Bases and Ones
- `sar_sub_chunking_a.pl`: Chunking subtraction strategy
- `smr_mult_c2c.pl`: Coordinating Two Counts for multiplication

**Theoretical Significance:** Each strategy is a formal proof that children's "informal" mathematical thinking has rigorous logical structure.

### Web Interface

**URL:** https://tiosavich.github.io/UMEDCTA/Calculator/index.html

**Startup:** Run `./start_system.sh` to launch local version

The web interface allows teachers and researchers to:
- Explore individual student strategies interactively
- See step-by-step visualizations of arithmetic processes
- Understand the cognitive structure behind student solutions

### Grounded Fractional Arithmetic System

**Files:** `jason.pl`, `composition_engine.pl`, `fraction_semantics.pl`, `grounded_ens_operations.pl`, `normalization.pl`

A comprehensive implementation of Jason's partitive fractional schemes using **nested unit representation** instead of rational numbers. This models how students actually think about fractions (as parts-of-wholes) rather than as ratios.

**Theoretical Significance:** Demonstrates that even advanced concepts like fractions can be grounded in embodied cognitive processes, supporting the manuscript's anti-Platonist stance.

### Critical Qualifications

**What the HC Does:**
- Provides a rigorous formalization showing how AI collaboration could be structured
- Models embodied cognitive strategies with crisis-driven learning
- Demonstrates that student thinking has formal logical structure
- Proves (via Gödel) that any such formalization is necessarily incomplete

**What the HC Does NOT Do:**
- Does not implement machine consciousness or self-awareness
- Cannot make genuine autonomous decisions about its foundational norms
- Does not participate in Hegelian *Geist* as a self-conscious agent
- Models the structure of mathematical consciousness without instantiating it

**Analogy:** A wind tunnel models flight dynamics but does not fly. The HC models mathematical consciousness but is not conscious.

---

## 2. LK_RB_Synthesis: Algorithmic Elaboration Discovery

**URL:** https://tiosavich.github.io/UMEDCTA/Calculator/LK_RB_Synthesis/

**Full README:** https://tiosavich.github.io/UMEDCTA/Calculator/LK_RB_Synthesis/README.md

### What It Does

The LK_RB_Synthesis system automatically discovers **algorithmic elaborations** between student arithmetic strategies. It analyzes Python automaton implementations to identify shared computational patterns and generate Meaning-Use Analysis (MUA) reports in the framework of Robert Brandom.

### Core Functions

#### Automated Pattern Discovery (AST Analysis)
**File:** `mud_generator.py`

Uses Abstract Syntax Tree parsing to identify computational patterns:
- **base_decomposition**: Breaking numbers into components (`//` and `%` operations)
- **incremental_counting**: State-based counting loops
- **iterative_arithmetic**: Repeated addition/subtraction
- **value_adjustment**: Target value calculations

**Theoretical Significance:** Reveals the implicit computational structure that students deploy when solving arithmetic problems, making explicit the "practices" that are "sufficient" for deploying mathematical "vocabulary" (Brandom's PV-sufficiency).

#### Algorithmic Elaboration Detection

Automatically discovers how strategies build upon each other. For example:
```
ADD_Counting → ADD_COBO → ADD_Chunking
    (via incremental counting pattern)

ADD_Rounding → ADD_RMB → ADD_COBO
    (via base decomposition pattern)
```

**Theoretical Significance:** Implements Brandom's concept of "algorithmic elaboration," where complex practices are systematically built from simpler prerequisite practices.

#### Rich Metadata Extraction

Extracts documentation from automata including:
- **Embodied Metaphors** (Lakoff & Núñez): Source/target domains and entailments
- **Material Inferences** (Brandom): Premises, conclusions, prerequisites
- **Visualization Hints**: Suggested cognitive representations
- **Deployed Vocabulary**: Key conceptual terms

#### Brandomian MUA Reports
**File:** `mua_report_generator.py`

Generates detailed Meaning-Use Analysis reports:
- **PV-Sufficiency**: What practices are sufficient to deploy vocabulary?
- **PP-Sufficiency**: What practices are sufficient for other practices?
- **VP-Sufficiency**: What vocabulary is sufficient for practices?
- **LX Relations**: Elaborated-Explicating relationships
- **Pragmatic Metavocabulary**: Analysis of how weaker vocabularies bootstrap stronger ones

**Example Output:** https://tiosavich.github.io/UMEDCTA/Calculator/LK_RB_Synthesis/output/mua_full_report.md

### Usage

```bash
# Run complete analysis
python3 main.py analyze

# List all strategies
python3 main.py list

# Generate report for specific strategy
python3 main.py report --strategy ADD_COBO
```

### Theoretical Significance

The LK_RB_Synthesis system provides computational evidence for the manuscript's claim that mathematical understanding develops through **pragmatic expressive bootstrapping**—the process by which simpler practices and vocabularies serve as the metavocabulary for articulating more complex mathematical concepts.

### Limitations

- Does not generate visual MUD diagrams (text reports only)
- Does not implement full Brandomian deontic scorekeeping
- Does not model Lakoff's conceptual metaphor mappings formally
- Analysis reveals structure but verification of philosophical claims requires human judgment

---

## 3. Interactive Web Interfaces

### The Calculator (Main Interface)

**URL:** https://tiosavich.github.io/UMEDCTA/Calculator/index.html

**What It Does:** Interactive web interface for exploring student arithmetic strategies. Features:
- Buttons for each strategy (COBO, Chunking, RMB, etc.)
- Real-time SVG visualizations of number lines and operations
- Step-by-step textual explanations
- Links to detailed PDF documentation for each strategy

**Theoretical Significance:** Allows teachers to develop what Habermas calls "practical knowledge"—understanding student thinking through interactive engagement rather than abstract theory.

**Styling:** https://tiosavich.github.io/UMEDCTA/Calculator/strategy_styles.css

### Ace of Bases

**URL:** https://tiosavich.github.io/UMEDCTA/Calculator/AceofBases/index.html

**What It Does:** Interactive canvas-based exploration of place value and number bases. Users:
- Drag to select cubes representing a grouping unit (base 2-15)
- Compose and decompose quantities
- See base conversion in real-time

**Theoretical Significance:** Demonstrates that place value is not a "fact" to memorize but a **constructed** understanding—users literally construct different base systems through embodied interaction with visual objects.

### More Zeeman: Catastrophe Machine

**URL:** https://tiosavich.github.io/UMEDCTA/More_Zeeman/index_unified.html

**What It Does:** Interactive visualization of the Zeeman Catastrophe Machine coupled with:
- **The Thinker (Zeeman Machine)**: Draggable control point affecting elastic bands, demonstrating catastrophe theory (sudden jumps in state due to smooth changes in parameters)
- **The Memory (More Machine)**: Matrix that grows via Cantorian diagonalization after each catastrophe
- **The Sound of Time (Acoustic Metaphor)**: Visual representation of air compression waves synchronized with the wheel's angular velocity

**Theoretical Significance:** Embodies three key manuscript themes:
1. **Catastrophe as consciousness**: Only discontinuous "memorable" events (catastrophes) trigger memory/matrix growth
2. **Diagonalization as self-transcendence**: The More Machine generates elements provably not in any finite list (Cantor's proof)
3. **The sound of time**: Angular velocity (change) creates "sound" (phenomenological experience of temporality)

**Technical Features:**
- Proper Hooke's Law physics with gradient descent
- User-adjustable spring parameters (stiffness, natural length, time speed)
- Hysteresis (system "remembers" its current state until forced to jump)

---

## 4. Philosophical Teaching Modules

### Inferential Strength (Brandom Module)

**URL:** https://tiosavich.github.io/UMEDCTA/Quadrilateral_Substitution/inferential_strength.html

**What It Does:** Interactive 7-module teaching sequence on Robert Brandom's argument for why singular terms must have symmetric substitution significance. Covers:

1. **Module 1**: Meaning as inferential role (Square ⇒ Rectangle)
2. **Module 2**: Incompatibility and inferential strength (interactive constraint relaxation)
3. **Module 3**: Substitution roles (substituted-for vs. frame)
4. **Module 4**: Polarity inversion (how logical contexts flip inferential relationships)
5. **Module 5**: The argument for symmetric terms (why "SquareTerm ⇒ RectangleTerm" leads to contradiction)
6. **Module 6**: Matrix of substitutional possibilities (ruling out three of four options)
7. **Module 7**: Conclusion and expressive deduction

**Theoretical Significance:** Makes Brandom's highly technical argument from *Articulating Reasons* Chapter 4 accessible through interactive exploration. Demonstrates that the structure of language (singular terms vs. predicates) is not arbitrary but required for logical expressiveness.

**Technical Features:**
- Live shape filtering in Module 2 (shapes transform as users relax constraints)
- Polarity inversion visualization in Module 4 (sliders showing strength relationships)
- Substitution animation in Module 5 (visual demonstration of the key argument)

---

## 5. How to Cite These Materials

### General Citation

```
Savich, T. (2025). UMEDCTA Supplementary Materials: Computational
Implementations for Understanding Mathematics as an Emancipatory
Discipline. https://tiosavich.github.io/UMEDCTA/
```

### Specific Components

**For the Hermeneutic Calculator (Prolog):**
```
Savich, T. (2025). The Hermeneutic Calculator: A Prolog Implementation
of Student Arithmetic Strategies with Incompatibility Semantics.
https://tiosavich.github.io/UMEDCTA/Calculator/Prolog/
```

**For LK_RB_Synthesis:**
```
Savich, T. (2025). LK_RB_Synthesis: Automated Algorithmic Elaboration
Discovery for Student Arithmetic Strategies.
https://tiosavich.github.io/UMEDCTA/Calculator/LK_RB_Synthesis/
```

**For Interactive Web Interfaces:**
```
Savich, T. (2025). Interactive Web Interfaces for Student Arithmetic
Strategies. https://tiosavich.github.io/UMEDCTA/Calculator/index.html
```

**For Philosophical Teaching Modules:**
```
Savich, T. (2025). Inferential Strength: An Interactive Guide to
Brandom's Argument for Singular Terms.
https://tiosavich.github.io/UMEDCTA/Quadrilateral_Substitution/inferential_strength.html
```

---

## Coherence with Manuscript Claims

### Critical Alignment Checklist

All supplementary materials must cohere with the manuscript's core philosophical commitments:

#### ✅ **Autoethnographic Method**
- HC born from author's memory of teaching
- Formalizes actual children's reasoning (not idealized algorithms)

#### ✅ **Critical Stance**
- Values error as "source of truth" (ORR cycle learns from failure)
- Respects subjective student strategies over formal correctness

#### ✅ **Hegelian Dialectic**
- ORR cycle implements determinate negation
- System recognizes its limits and transcends them
- "Built to break" philosophy in fragile formalizations

#### ✅ **Brandomian Inferentialism**
- Incompatibility semantics implemented computationally
- Algorithmic elaboration discovery in LK_RB_Synthesis
- Material inferences grounded in practices

#### ✅ **Habermasian Emancipation**
- Serves practical-hermeneutic interest (teacher understanding)
- Provides technical models without claiming they're "complete"
- Documentation acknowledges system limits

#### ✅ **Numerals as Pronouns**
- Numbers represented as recollection structures (`s(s(s(0)))`)
- Grounded in successor function (not abstract objects)
- Models first-person "I think" as computational trace

#### ✅ **Incompleteness as Becoming**
- System can detect its own limitations (ORR cycle)
- "More Machine" implements diagonalization
- Documentation explicitly states formalization is incomplete

---

## Technical Requirements

### For Local Development

**Prolog System:**
- SWI-Prolog 8.0+
- Run: `./start_system.sh` in Calculator/Prolog/

**Python Analysis:**
- Python 3.8+
- Run: `pip install -r requirements.txt` in Calculator/LK_RB_Synthesis/

**Web Interfaces:**
- Any modern browser
- No build process required (vanilla HTML/CSS/JS)

### For Manuscript Integration

When citing these materials in the manuscript:

1. **Use specific URLs** for each component (not just the repository root)
2. **Reference specific files** when discussing technical details (e.g., "`incompatibility_semantics.pl` implements Brandom's logic...")
3. **Acknowledge limitations** (e.g., "The HC models consciousness without instantiating it...")
4. **Explain philosophical significance** (e.g., "The ORR cycle demonstrates that...")

---

## Questions and Contributions

For questions about these materials, open an issue at:
https://github.com/TioSavich/UMEDCTA/issues

For the manuscript itself, contact the author directly.

---

**Last Updated:** 2025-10-12
**Version:** 1.0
**License:** [Specify license]
