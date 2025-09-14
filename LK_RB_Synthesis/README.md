# Embodied Pragmatic Logic Engine (EPLE)

## Project Overview

This project, the Embodied Pragmatic Logic Engine (EPLE), is a novel system designed to synthesize two powerful philosophical and cognitive science frameworks:

* **George Lakoff and Rafael Núñez's Embodied Mathematics:** As detailed in "Where Mathematics Comes From," this framework posits that mathematical concepts are grounded in everyday embodied experiences, which are structured by image schemas and conceptual metaphors.
* **Robert Brandom's Analytic Pragmatism:** From "Between Saying and Doing," this framework provides a formal structure for understanding how the meaning of concepts is rooted in their practical use in language and action.

The central goal of EPLE is to create a computational model that demonstrates how abstract mathematical vocabularies derive their meaning and necessity from embodied practices.

### Core Thesis

The project's foundational thesis is:

> Conceptual metaphors (from Lakoff & Núñez) function as the mechanisms of pragmatic elaboration (from Brandom) that allow embodied practices to confer content on abstract mathematical vocabularies. Mathematical necessity is the explicit expression of the constraints inherent in those embodied practices.

### System Architecture

EPLE is designed with a two-layer architecture:

1.  **The MUA Layer (The Form):** This is the analytic engine that implements Brandom's Meaning-Use Analysis (MUA). It reasons about the relationships between "saying" (vocabularies) and "doing" (practices).
2.  **The Content/Prover Layer (The Substance):** This layer contains the inference engine and the concrete implementations of practices:
    * **Logic Engine:** An evolution of the `incompatibility_semantics.py` prover, this engine focuses on deontic scorekeeping (tracking commitments and entitlements).
    * **Embodiment Engine:** This component models image schemas and conceptual metaphors from Lakoff & Núñez's work.
    * **Choreography Engine:** This engine uses executable automata to model cognitive strategies, as described in the HC_GEM documents.

### The Hybrid AI Core

A key feature of EPLE is its hybrid AI core, which creates an interaction loop between a Large Language Model (LLM) and the formal system:

* **Pragmatic Projection (LLM):** The LLM is used for non-algorithmic tasks, such as proposing new metaphors and providing intuitive insights to resolve logical incoherencies.
* **Algorithmic Elaboration (Formal System):** The formal system is responsible for the verification and execution of the LLM's proposals, ensuring that they are consistent with the embodied groundings of the system.

This project is an ambitious attempt to bridge the gap between the philosophy of language, cognitive science, and artificial intelligence. By creating a working model of the EPLE, we aim to provide a powerful new tool for understanding the nature of mathematical reasoning and a novel architecture for building more grounded and flexible AI systems.