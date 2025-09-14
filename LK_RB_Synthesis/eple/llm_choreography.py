"""
llm_choreography.py: Demonstrates the integration of the eple model with a
Large Language Model (LLM) to reason about cognitive development.
"""

import sys
import os

# Add project root to path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from eple.core.deontic_scorekeeper import DeonticScorekeeper
from eple.domains.embodiment.object_manipulation import P_ObjectManipulation
from eple.domains.arithmetic.base import P_Arithmetic
from eple.domains.arithmetic.core import P_ArithmeticAsObjectCollection, ArithmeticIsObjectCollection
from eple.domains.arithmetic.strategies import P_CountingOn, P_RMB
from eple.core.mua import AlgorithmicElaboration, PP_Sufficiency

def get_all_murs():
    """Gathers all Meaning-Use Relations defined in the project."""
    # Elaboration of RMB from CountingOn
    rmb_elaboration = AlgorithmicElaboration(P_base=P_CountingOn, P_elaborated=P_RMB)
    
    # Elaboration of CountingOn from the metaphor
    strategy_elaboration = AlgorithmicElaboration(
        P_base=P_ArithmeticAsObjectCollection,
        P_elaborated=P_CountingOn
    )
    
    # Elaboration of the metaphor from the base arithmetic practice
    metaphorical_elaboration = PP_Sufficiency(
        P_base=P_Arithmetic,
        P_elaborated=P_ArithmeticAsObjectCollection
    )
    
    return {
        rmb_elaboration,
        strategy_elaboration,
        metaphorical_elaboration,
        ArithmeticIsObjectCollection # The projection itself
    }

def get_all_practices():
    """Gathers all Practice objects defined in the project."""
    return {
        P_ObjectManipulation,
        P_Arithmetic,
        P_ArithmeticAsObjectCollection,
        P_CountingOn,
        P_RMB
    }

def format_incompatibilities(practice, scorekeeper):
    """Formats the incompatibilities of a practice for inclusion in a prompt."""
    incomps = scorekeeper.get_incompatibilities(practice.name)
    if not incomps:
        return f"  - No explicit incompatibilities defined for {practice.name}."
    
    formatted = [f"  - {incomp}" for incomp in incomps]
    return "\n".join(formatted)

def generate_llm_prompt(scorekeeper):
    """
    Generates a rich prompt for an LLM, including formal rules from the model.
    """
    prompt = """
You are an expert in cognitive development and mathematics education.
Your task is to analyze a child's problem-solving process based on a formal
model of their understanding.

**Scenario:**
A child is solving the addition problem: 8 + 5.
The child has mastered the 'Counting On' strategy but is now learning the
more advanced 'Rearranging-to-Make-Bases' (RMB) strategy, which we can also
call 'Make-a-Ten'.

**Formal Model Context:**
The child's understanding is modeled by a set of practices, each with its own
vocabulary and rules (incompatibilities).

1.  **P_ArithmeticAsObjectCollection (Metaphorical Understanding):**
    This is the child's foundational, intuitive understanding of numbers as
    collections of objects. Its rules are projected from the physical manipulation
    of objects.
    *Incompatibilities:*
{incomps_metaphor}

2.  **P_CountingOn (Procedural Knowledge):**
    This is the child's well-understood, step-by-step procedure for addition.
    *Incompatibilities:*
{incomps_counting_on}

3.  **P_RMB (Emerging Strategy):**
    This is the new, more complex strategy the child is learning. It builds on
    the metaphorical understanding of making and breaking collections.
    *Incompatibilities:*
{incomps_rmb}

**Question:**
Given this formal model, describe the cognitive steps the child might take.
What errors might they make? How does the 'Make-a-Ten' (RMB) strategy
represent a development from 'Counting On', and how does it connect to the
foundational 'Object Collection' metaphor?
    """.format(
        incomps_metaphor=format_incompatibilities(P_ArithmeticAsObjectCollection, scorekeeper),
        incomps_counting_on=format_incompatibilities(P_CountingOn, scorekeeper),
        incomps_rmb=format_incompatibilities(P_RMB, scorekeeper)
    )
    return prompt

def simulate_llm_response(prompt):
    """
    Simulates a response from an LLM that has processed the rich prompt.
    """
    # In a real application, this would be an API call to an LLM.
    # The hardcoded response demonstrates the *kind* of output this hybrid
    # approach enables.
    
    simulated_response = """
Excellent, this formal model provides a robust framework for analyzing the child's thinking. Here is my assessment based on the provided context:

**Analysis of the Child's Cognitive Process (Solving 8 + 5):**

1.  **Initial State & Conflict:** The child knows `Counting On` but is being encouraged to use `Make-a-Ten`. `Counting On` is reliable but inefficient for this problem (counting 5 steps). `Make-a-Ten` is more efficient but cognitively demanding as it's a new procedure.

2.  **Leveraging the Metaphor:** The `Make-a-Ten` (RMB) strategy is grounded in the `P_ArithmeticAsObjectCollection` practice. The incompatibility rule `Incompatible(Subtract(y, x, z), LessThan(y, x))`—which you've shown is projected from the physical rule that you can't take a whole from its part—is key here. The child intuitively understands you can't take 5 from 2, for example. The RMB strategy involves decomposing a number (5) into parts (2 and 3), which is a sophisticated application of this part-whole understanding.

3.  **Possible Cognitive Steps & Errors:**
    *   **Step 1 (Goal Setting):** The child recognizes the goal is to make the '8' into a '10'. This is the core of the RMB strategy.
    *   **Step 2 (Decomposition):** The child needs to figure out how much to take from '5' to make '10'. This requires knowing that 8 + 2 = 10. This is a `PartOf(2, 10)` relation in the source domain.
    *   **Step 3 (Error Prone):** The child must now partition the '5' into '2' and '3'. This is a common point of error. They might forget the remainder, or miscalculate it. For instance, they might correctly take 2 from 5 but then forget the remaining 3, answering "10". This would be a failure to follow the `Combine` inference from the object manipulation practice.
    *   **Step 4 (Recomposition):** The child performs `(8 + 2) + 3`. The first part, `8 + 2`, results in the 'base' (10). The final step is `10 + 3`, which is typically much easier.

**Development from 'Counting On' to 'RMB':**

*   **From Unitary to Composite:** `Counting On` treats numbers as a sequence of single units. `RMB` treats numbers as composite wholes that can be broken apart and reassembled (`PartOf`, `Combine`). This is a major conceptual leap.
*   **Explicating Implicit Knowledge:** The `is_LX` analysis you performed shows that `RMB` is an elaboration that makes the part-whole structure of arithmetic *explicit*. The `Counting On` procedure doesn't require a deep understanding of part-whole relations, but `RMB` absolutely does.
*   **Connection to Metaphor:** `RMB` is a formal procedure that directly operationalizes the intuitions from the `Arithmetic as Object Collection` metaphor. The act of "taking" a part of the '5' and "giving" it to the '8' is a direct cognitive choreography of the physical act of moving objects between collections to create a new, neater collection (a full ten).

In summary, the formal model shows that learning the `Make-a-Ten` strategy is not just about memorizing new steps; it's about developing a deeper, more explicit understanding of the part-whole relationships that were only implicit in earlier, simpler procedures. The model correctly identifies the potential for error in the complex choreography of decomposing and recomposing numbers.
"""
    return simulated_response


if __name__ == '__main__':
    # 1. Initialize the Deontic Scorekeeper
    all_practices = get_all_practices()
    all_murs = get_all_murs()
    scorekeeper = DeonticScorekeeper(practices=all_practices, murs=all_murs)

    # 2. Generate the LLM prompt
    prompt = generate_llm_prompt(scorekeeper)

    # 3. Get the simulated LLM response
    response = simulate_llm_response(prompt)

    # 4. Display the results
    print("="*80)
    print("                        Hybrid AI Cognitive Choreography")
    print("="*80)
    print("\n--- Generated LLM Prompt ---")
    print(prompt)
    print("\n" + "="*80)
    print("\n--- Simulated LLM Response ---")
    print(response)
    print("\n" + "="*80)
    print("\nProject `eple` is complete.")
    print("This demonstration shows how the formal MUA model can provide structured,")
    print("verifiable context to an LLM, enabling more grounded and insightful")
    print("reasoning about complex cognitive phenomena.")

