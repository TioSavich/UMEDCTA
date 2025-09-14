This document provides the instructions for a VS Code Agent to develop the `synthesis_lk_rb` project, hereafter referred to as the Embodied Pragmatic Logic Engine (EPLE). This system will synthesize Lakoff & Núñez's embodied mathematics (L\&N) with Robert Brandom's analytic pragmatism (BSD).

The goal is to evolve the existing `incompatibility_semantics.py` into a sophisticated, hybrid system that grounds mathematical necessity in embodied practices, utilizing Brandom's Meaning-Use Analysis (MUA) as the formal structure, and integrating an LLM to facilitate pragmatic elaboration.

-----

## Project Vision: The Embodied Pragmatic Logic Engine (EPLE)

The objective is to build a computable model demonstrating how abstract mathematical concepts (Vocabularies) are derived from embodied actions (Practices) through conceptual metaphor.

The central thesis (from `synthesis_lk_rb.md`) is: **Conceptual metaphors (L\&N) function as the mechanisms of pragmatic elaboration (PP-sufficiency, BSD) that allow embodied practices to confer content on abstract mathematical vocabularies. Mathematical necessity is the explicit expression (LX) of the constraints inherent in those embodied practices.**

## Architecture Overview

The EPLE will be implemented as a two-layer architecture, as analyzed in `gemini_ideas_for_synthesis.md`:

1.  **The MUA Layer (The Form):** The analytic engine implementing Brandom's metavocabulary (V, P, MURs, LX). This layer reasons *about* the relationship between saying and doing.
2.  **The Content/Prover Layer (The Substance):** The inference engine and the implementation of practices.
      * **Logic Engine:** Evolved from `incompatibility_semantics.py`, focusing on deontic scorekeeping (Commitment/Entitlement).
      * **Embodiment Engine:** Models of Image Schemas and Conceptual Metaphors (L\&N).
      * **Choreography Engine:** Executable automata modeling cognitive strategies (HC\_GEM).

**The Hybrid AI Core:** An interaction loop where an LLM handles **Pragmatic Projection** (non-algorithmic insight, metaphor proposal) and the formal system handles **Algorithmic Elaboration** (verification and execution).

## Agent Instructions: Phased Implementation

### Phase 0: Project Setup and Structure

1.  **Analyze Inputs:** Review all provided documents (`synthesis_lk_rb.md`, `incompatibility_semantics.py`, `gemini_ideas_for_synthesis.md`, `lakoff_*.md`, `HC_GEM.pdf`, `Baby AI Anti_Gauss.ipynb`).

2.  **Initialize Project Structure:** Set up the following directory structure:

    ```
    eple/
    ├── core/
    │   ├── logic_terms.py          # Term, Predicate, Sequent definitions
    │   ├── incompatibility_engine.py # Core prover (Deontic Scorekeeper)
    │   ├── mua.py                  # MUA framework (V, P, MURs, LX analysis)
    │   └── metaphor.py             # Conceptual Metaphor mechanism
    ├── domains/
    │   ├── embodiment/
    │   │   ├── schemas.py          # Image Schemas (Container, Motion)
    │   │   └── object_manipulation.py
    │   ├── arithmetic/
    │   │   ├── core.py             # N, Z, Q, C domains
    │   │   └── strategies.py       # HC_GEM Automata (RMB, COBO)
    │   └── geometry.py
    ├── hybrid_ai/
    │   ├── llm_interface.py
    │   └── projection_mediator.py  # LLM loop for projective elaboration
    └── tests/
    ```

3.  **Initial Refactoring:**

      * Move `Term`, `Predicate`, `Sequent`, etc., from `incompatibility_semantics.py` to `eple/core/logic_terms.py`.
      * Move the `IncompatibilitySemantics` class to `eple/core/incompatibility_engine.py`.

### Phase 1: The Pragmatic Logic Engine (Refactoring and Deontic Scorekeeping)

Evolve the logic engine from a simple prover to a pragmatic scorekeeper.

1.  **Decouple Domain Knowledge (in `eple/core/incompatibility_engine.py`):**
      * Refactor the engine to remove hardcoded domain knowledge (Geometry, Arithmetic, EML).
      * The engine must rely on the currently active `Practice` (defined in the MUA layer) to provide the rules for material inference and incompatibility.
2.  **Implement Deontic Scorekeeping:**
      * This is central to Brandom's pragmatism. Introduce explicit handling of normative statuses.
      * Introduce predicates (using `logic_terms.py`) for `Commits(Agent, Proposition)` and `Entitled(Agent, Proposition)`.
      * Refactor the engine's core function. Instead of just checking provability, it must track how commitments propagate and how entitlements are inherited according to the material inferences defined by the active practices.

### Phase 2: The MUA Framework (The Analytic Machinery)

Implement Brandom's Meaning-Use Analysis (MUA) framework in `eple/core/mua.py`.

1.  **Implement Core MUA Classes:**
      * `Vocabulary (V)`: Represents the "Saying."
      * `Practice (P)`: Represents the "Doing." This must link to concrete implementations (e.g., the rules in `domains/` or the automata in Phase 3).
2.  **Implement Meaning-Use Relations (MURs):**
      * Implement `PV_Sufficiency`, `VP_Sufficiency`, `PV_Necessity`.
      * Implement `PP_Sufficiency(P1, P2, mechanism)`. Crucially, define the mechanism types:
          * `AlgorithmicElaboration`: Deterministic transformations (e.g., evolving one automaton into another).
          * `PragmaticProjection`: Non-algorithmic, metaphorical extension (mediated by the LLM in Phase 5).
3.  **Implement MUA Analysis Functions:**
      * Implement MUR composition.
      * Implement `find_pragmatic_metavocabulary(V_target)`.
      * Implement the LX (Elaborated-Explicating) relation checker: `is_LX(V_explicating, V_elaborated)`.

### Phase 3: Embodied Practices and Choreography (L\&N and HC\_GEM)

Ground the abstract practices in executable models of embodied cognition.

1.  **Define the Embodied Core (in `eple/domains/embodiment/`):**
      * Implement Image Schemas as foundational `Practices`.
      * `P_ContainerSchema` (in `schemas.py`): Define the logic of containment (In/Out incompatibility, Transitivity). This grounds logical negation and modus ponens.
      * `P_ObjectManipulation` (in `object_manipulation.py`): Define the logic of collection (constraints on taking away larger from smaller).
2.  **Implement Cognitive Choreographies (in `eple/domains/arithmetic/strategies.py`):**
      * Formalize the automata from `HC_GEM.pdf`. Implement a base class for cognitive automata (e.g., `RegisterMachine`).
      * Implement specific strategies (e.g., `CountingOn`, `RMB`, `COBO`).
      * These automata are the executable implementations of arithmetic `Practices`.
3.  **Model HC\_GEM Evolution:**
      * Use the MUA layer to model the "Fractal Architecture" (HC\_GEM). Show how complex strategies (e.g., `RMB`) are `AlgorithmicElaborations` of simpler ones (`CountingOn`) through temporal compression/decompression.

### Phase 4: Metaphorical Elaboration and Domain Shifts

Implement conceptual metaphors as the mechanisms of PP-Sufficiency that transfer inference from embodied domains to abstract ones.

1.  **Implement the Metaphor Mechanism (in `eple/core/metaphor.py`):**
      * Define `ConceptualMetaphor(SourceDomain, TargetDomain, Mapping)`.
      * This class must actively propagate the inferential structure (incompatibilities and entailments) from source to target.
2.  **Implement Grounding Metaphors (AOC):**
      * Implement **Arithmetic is Object Collection (AOC)**.
      * Define the MUA relation: `PP_Sufficiency(P_ObjectManipulation, P_Arithmetic_N, mechanism=AOC_Metaphor)`.
      * **Crucial Demonstration:** The system must derive the constraints of arithmetic (e.g., the impossibility of 3-5 in N) directly from the constraints of `P_ObjectManipulation` propagated via AOC.
3.  **Model Domain Extension (N to Z):**
      * Implement the mechanism for domain shifting. Model how the incoherence of `3-5` under AOC necessitates new metaphors.
      * Implement "Numbers Are Points on a Line" (NPL) and "Multiplication by -1 is Rotation."
      * The prover must update its rules based on the new metaphors, grounding the necessity of `(-1)*(-1)=1` in the spatial logic of 180° rotation.

### Phase 5: The Hybrid AI Core (LLM Integration)

Integrate the LLM to handle non-algorithmic `PragmaticProjection`, inspired by `Baby AI Anti_Gauss.ipynb`.

1.  **LLM Interface (in `eple/hybrid_ai/llm_interface.py`):**

      * Implement a standardized interface for querying the LLM.

2.  **Pragmatic Projection Mediator (in `eple/hybrid_ai/projection_mediator.py`):**

      * This module manages the interaction between the MUA framework, the Prover, and the LLM when a projection is needed.
      * **The Elaboration Loop:**
        1.  **Identify Incoherence:** The Prover identifies an incoherence (e.g., `3-5` in Domain N).
        2.  **Query LLM (Intuition):** The Mediator prompts the LLM to propose an elaboration (a new metaphor or practice) to resolve the incoherence, providing the MUA context.
        3.  **LLM Proposal:** The LLM proposes a conceptual metaphor (e.g., NPL).
        4.  **Formalization & Verification (The Checker):** The Mediator attempts to formalize the proposal using `ConceptualMetaphor`, and the `incompatibility_engine` verifies if it resolves the incoherence and maintains consistency with embodied groundings.
        5.  **Feedback:** Iterate with the LLM if verification fails.

3.  **Theorem Proving:** The final goal is to use this loop to prove theorems by demonstrating how a statement is the LX consequence of a chain of elaborated embodied practices, navigated by the LLM and verified by the formal system.