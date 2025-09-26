"""
strategy_as_elaboration.py: Models and tests the elaboration of an arithmetic
strategy (CountingOn) from the metaphorical practice of 'Arithmetic as Object Collection'.
"""

import sys
import os

# Add project root to path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../..')))

from eple.core.mua import AlgorithmicElaboration, is_LX
from eple.domains.arithmetic.core import P_ArithmeticAsObjectCollection, V_Arithmetic
from eple.domains.arithmetic.strategies import P_CountingOn, V_CountingOn

def run_strategy_elaboration_test():
    """
    Tests if P_CountingOn is a Lexical Expansion of the metaphorical practice.
    """
    print("--- Testing Strategy as Metaphorical Elaboration ---")

    # 1. Define the Elaboration Relation
    # This MUR states that the CountingOn practice is a sufficient elaboration
    # of the metaphorical arithmetic practice.
    StrategyElaboration = AlgorithmicElaboration(
        P_base=P_ArithmeticAsObjectCollection,
        P_elaborated=P_CountingOn
    )

    # 2. Define the universe of discourse for the MUA
    all_practices = {P_ArithmeticAsObjectCollection, P_CountingOn}
    all_murs = {StrategyElaboration}

    # 3. Run the Lexical Expansion Test
    is_lexical_expansion = is_LX(
        p_base=P_ArithmeticAsObjectCollection,
        p_elaborated=P_CountingOn,
        v_base=V_Arithmetic, # The base vocabulary is the general arithmetic one
        v_elaborated=V_CountingOn,
        all_practices=all_practices,
        all_murs=all_murs
    )

    print(f"\nBase Practice (Metaphor): {P_ArithmeticAsObjectCollection.name}")
    print(f"Elaborated Practice (Strategy): {P_CountingOn.name}")
    print(f"\nIs '{P_CountingOn.name}' a Lexical Expansion of '{P_ArithmeticAsObjectCollection.name}'?")
    print(f"Result: {is_lexical_expansion}")

    if is_lexical_expansion:
        print("\n✅ Test Passed: The strategy is a valid elaboration of the metaphor.")
        print("This demonstrates how a formal procedure (CountingOn) can be grounded")
        print("in an embodied, metaphorical understanding of arithmetic.")
    else:
        print("\n❌ Test Failed: The strategy is NOT a valid elaboration of the metaphor.")
        print("This suggests a mismatch between the rules of the strategy and the")
        print("foundational metaphorical practice.")

if __name__ == '__main__':
    run_strategy_elaboration_test()
