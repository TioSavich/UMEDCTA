"""
metaphor_test.py: Verifies that the 'Arithmetic is Object Collection' metaphor
is a valid Lexical Expansion (LX) of the base arithmetic practice.
"""

import sys
import os

# Add project root to path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../..')))

from eple.core.mua import is_LX, PP_Sufficiency
from eple.domains.embodiment.object_manipulation import P_ObjectManipulation
from eple.domains.arithmetic.base import P_Arithmetic, V_Arithmetic
from eple.domains.arithmetic.core import (
    P_ArithmeticAsObjectCollection,
    V_Arithmetic as V_ArithmeticMetaphor, # The vocabulary of the metaphorical practice
    ArithmeticIsObjectCollection
)

def run_metaphor_test():
    """
    Tests if P_ArithmeticAsObjectCollection is a Lexical Expansion of P_Arithmetic.
    """
    print("--- Testing 'Arithmetic is Object Collection' Metaphor ---")

    # This MUR states that the metaphorical practice is a sufficient elaboration of the base one.
    # This is the link the previous test was missing.
    MetaphoricalElaboration = PP_Sufficiency(P_base=P_Arithmetic, P_elaborated=P_ArithmeticAsObjectCollection)

    # Define the universe of discourse for the MUA
    all_practices = {P_Arithmetic, P_ObjectManipulation, P_ArithmeticAsObjectCollection}
    # Both the projection and the elaboration relation are needed for the full analysis
    all_murs = {ArithmeticIsObjectCollection, MetaphoricalElaboration}

    # The 'is_LX' function checks if the elaborated practice/vocabulary pair
    # is a lexical expansion of the base practice/vocabulary pair.
    is_lexical_expansion = is_LX(
        p_base=P_Arithmetic,
        p_elaborated=P_ArithmeticAsObjectCollection,
        v_base=V_Arithmetic,
        v_elaborated=V_ArithmeticMetaphor,
        all_practices=all_practices,
        all_murs=all_murs
    )

    print(f"\nBase Practice: {P_Arithmetic.name}")
    print(f"Elaborated Practice (Metaphor): {P_ArithmeticAsObjectCollection.name}")
    print(f"\nIs '{P_ArithmeticAsObjectCollection.name}' a Lexical Expansion of '{P_Arithmetic.name}'?")
    print(f"Result: {is_lexical_expansion}")

    if is_lexical_expansion:
        print("\n✅ Test Passed: The metaphor is a valid Lexical Expansion.")
        print("This means the metaphorical practice introduces new vocabulary and rules")
        print("in a way that is consistent with the base arithmetic practice.")
    else:
        print("\n❌ Test Failed: The metaphor is NOT a valid Lexical Expansion.")
        print("This indicates an inconsistency between the projected rules from the")
        print("source domain and the existing rules of the target domain.")

if __name__ == '__main__':
    run_metaphor_test()
