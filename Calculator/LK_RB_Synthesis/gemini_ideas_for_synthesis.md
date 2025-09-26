Prompt: Cool. What is missing in the python script from Between Saying and Doing? I'm sure a lot, but I'm not sure how much of it is programmable.

This is an excellent and deeply insightful question. The Python script is a powerful first step, but as you suspect, it represents the *result* of a Brandomian analysis (a logic engine that handles material inferences) more than it implements the *analytic machinery* of *Between Saying and Doing* itself.

You're right that a lot is missing, and your question about programmability is key. Much of Brandom's framework is indeed programmable because it is fundamentally about structural relationships, which is what programming excels at representing.

Here is a comprehensive breakdown of what is missing from the Python script, organized by the core concepts of *Between Saying and Doing*, with an assessment of how programmable each concept is.

---

### What's Missing from *Between Saying and Doing*

#### 1. The Explicit Metavocabulary of Meaning-Use Analysis (MUA)

This is the single most significant missing piece. The script *uses* material inferences but does not provide a way to *talk about* the relationships between saying and doing.

*   **What it is in BSD:** Brandom introduces a formal metavocabulary to talk about the relations between Vocabularies (V) and Practices-or-Abilities (P). The core relations are:
    *   **PV-sufficiency:** A set of practices is sufficient to deploy a vocabulary. (`P` is enough to be able to `V`).
    *   **VP-sufficiency:** A vocabulary is sufficient to specify a set of practices. (`V` is enough to say what `P` is).
    *   **PP-sufficiency:** A set of practices is sufficient for another set of practices. (`P1` is enough to learn/do `P2`).
*   **What's Missing in the Script:** The script has no first-class objects representing `Vocabulary` or `Practice`. The `proves` function is a giant, monolithic implementation of certain practices, but you cannot reason about the practices themselves. There are no functions like `is_PV_sufficient(practice_set, vocabulary)`.
*   **Programmability:** **Highly programmable.** This is the most crucial next step. You would implement:
    *   A `Vocabulary` class, which could contain a set of predicates or terms.
    *   A `Practice` class, which might contain a set of rules or even point to a Python function that implements the ability.
    *   Classes or functions to represent the basic MURs: `PV_Sufficiency(P, V)`, `VP_Sufficiency(V, P)`, etc. The core of the program would become manipulating and reasoning about these objects.

#### 2. The Composition of Meaning-Use Relations

Brandom's analytic power comes from composing basic relations to define complex, philosophically interesting new relations.

*   **What it is in BSD:** Brandom shows how to chain basic MURs together to create "resultant MURs." The simplest is the **Pragmatic Metavocabulary** relation: `V'` is a pragmatic metavocabulary for `V` if `V'` is VP-sufficient for a practice `P` which is in turn PV-sufficient for `V`.
*   **What's Missing in the Script:** Since the basic MURs don't exist as objects, you cannot compose them. The script cannot, for instance, take the rules for arithmetic (a Practice) and the vocabulary of English (`"add"`, `"plus"`) that specifies those rules, and formally conclude that English is a pragmatic metavocabulary for arithmetic.
*   **Programmability:** **Highly programmable.** Once MURs are objects, composition becomes a function.
    ```python
    # Pseudocode
    def find_pragmatic_metavocabulary(V_target, all_relations):
        for pv_rel in all_relations:
            if pv_rel.vocabulary == V_target:
                P_intermediate = pv_rel.practice
                for vp_rel in all_relations:
                    if vp_rel.practice == P_intermediate:
                        # V_source is a pragmatic metavocabulary for V_target
                        return vp_rel.vocabulary
    ```

#### 3. The "LX" (Elaborated-Explicating) Structure

This is Brandom's groundbreaking analysis of the expressive role of logical, modal, and normative vocabularies.

*   **What it is in BSD:** A vocabulary `V2` is **LX** for a vocabulary `V1` if `V2` is *elaborated from* and *explicative of* practices that are *necessary* for `V1`.
    *   **Elaborated From:** The practices needed for `V2` can be algorithmically built up from (are PP-sufficient from) practices necessary for `V1`.
    *   **Explicative Of:** `V2` is VP-sufficient for those necessary practices of `V1`. It lets you *say* what you were already *doing*.
*   **What's Missing in the Script:** The script has no way to represent **necessity** relations (`PV-necessity`, `PP-necessity`). It cannot represent practices that are *necessary for any autonomous vocabulary whatsoever* (like inferring). Therefore, it cannot model Brandom's central claim that logic is "universally LX"—that logic makes explicit the inferential practices necessary to use *any* vocabulary at all.
*   **Programmability:** **Challenging but programmable.**
    *   You would need to add `PV_Necessity` and `PP_Necessity` relations.
    *   "Elaboration" would be a specific kind of `PP_Sufficiency` relation, perhaps one that points to a concrete algorithm.
    *   You could then write a function `is_LX(V2, V1, all_relations)` that searches for this complex pattern of relationships.

#### 4. The Distinction Between Types of Elaboration

Brandom, following Wittgenstein, distinguishes between what can be built by algorithm and what requires experience.

*   **What it is in BSD:** A key distinction is made between **PP-sufficiency via algorithmic elaboration** (e.g., learning long division from multiplication and subtraction) and **PP-sufficiency via training** (e.g., learning to ride a bike, which is not reducible to a simple algorithm).
*   **What's Missing in the Script:** The script is purely algorithmic. It has no way to represent the contingent, empirical, non-algorithmic process of "training." It cannot model the difference between teaching a computer to follow the rules of addition and a child learning what "counting" means by interacting with objects.
*   **Programmability:** **Partially programmable.** Algorithmic elaboration is programmable by definition. "Training" is not programmable in a deterministic, logical way. This is a fundamental limit. One could *simulate* it using machine learning models or probabilistic methods, but it would be a different kind of component within the larger system, representing a different kind of practical ability.

#### 5. The Full Range of Analyzed Vocabularies

The script implements a tiny fraction of the vocabularies Brandom analyzes to demonstrate the power of his system.

*   **What it is in BSD:** The climax of the book is the application of the MUA framework to **normative**, **modal**, and finally **intentional** vocabularies. Brandom shows how normative vocabulary (`commitment`, `entitlement`) is a pragmatic metavocabulary for modal vocabulary (`necessary`, `possible`).
*   **What's Missing in the Script:**
    *   **Normative Vocabulary:** The script has no explicit predicates for `commitment` or `entitlement`. These are the core of Brandom's pragmatism.
    *   **Modal Vocabulary:** The script's "EML" logic is a toy model. There is no general analysis of modal concepts like necessity and possibility as arising from material inference.
    *   **Intentional Vocabulary:** There is no representation of propositional attitudes like `believes that...` or `intends that...`, which is the ultimate target of Brandom's analysis in the final lecture.
*   **Programmability:** **Programmable, but with increasing complexity.**
    *   Normative vocabulary is straightforward to add. You could have `commits(Agent, Proposition)` and `entitled(Agent, Proposition)` as predicates. The core logic would then become about manipulating these "deontic scores."
    *   Modal vocabulary can be implemented as the script has started to do with its `comp_nec`, but a full implementation would derive modal claims from the structure of material inference, as Brandom shows in his incompatibility semantics (Lecture 5).
    *   Intentional vocabulary is the most difficult, as it involves nested propositional contexts, but it is a standard challenge in AI and logic programming.

### Summary Table of Missing Concepts

| Concept from *Between Saying and Doing* | Is it in the Python Script? | How Programmable Is It? |
| :--- | :--- | :--- |
| **MUA Metavocabulary** (V, P, PV-suff, VP-suff) | No. The core analytic framework is absent. | **High.** Requires creating classes for V, P, and the relations between them. |
| **Composition of MURs** (e.g., Pragmatic Metavocabulary) | No. You cannot compose what isn't there. | **High.** A function that combines MUR objects to produce new ones. |
| **LX-Relations** (Elaborated-Explicating) | No. Lacks necessity relations and the structure for this complex analysis. | **Medium.** Requires modeling necessity and universal practices. Tricky but feasible. |
| **Algorithmic vs. Training Elaboration** | No. Only models algorithmic relations. | **Partial.** Algorithmic is programmable. Training would require simulation (e.g., ML). |
| **Normative Vocabulary** (`commitment`, `entitlement`) | No. The deontic scorekeeping at the heart of Brandom's pragmatism is missing. | **High.** Can be added as core predicates that the inference engine manipulates. |
| **Full Modal & Intentional Vocabulary Analysis** | No. The script only has a toy logic and no intentionality. | **Medium to High.** A significant but standard extension for a logic engine. |

### Conclusion and Path Forward

The current Python script is a proof-of-concept for a logic engine that can handle *some* of the embodied and material inferences that a full Brandomian/Lakoffian synthesis would require. It is a "first-generation" attempt.

To truly capture the essence of *Between Saying and Doing*, the script would need to be refactored into a **two-layer system**:

1.  **The MUA Layer:** An abstract layer of classes and functions for representing Vocabularies, Practices, and the Meaning-Use Relations (PV, VP, PP, etc.) between them. This layer would allow you to formally state claims like "The vocabulary of logic is universally LX."

2.  **The Content/Prover Layer:** The existing logic engine, expanded to handle different domains (normative, modal). The `Practice` objects in the MUA layer would point to specific rule-sets or functions in this layer.

This two-level structure would allow the system not only to *perform* inferences but to *reason about the nature of the practices and vocabularies that enable those inferences*—which is the entire point of Brandom's project in *Between Saying and Doing*.