
import sys
import os

# Add the project root to the Python path
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..'))
sys.path.insert(0, project_root)

from eple.core.logic_terms import Predicate
from eple.core.mua import Vocabulary, Practice, PragmaticProjection
from eple.domains.embodiment.object_manipulation import V_ObjectManipulation, P_ObjectManipulation

# --- Target Vocabulary: Arithmetic ---

# Predicates for basic arithmetic
Add = Predicate("Add", 3)       # Add(a, b, result)
Subtract = Predicate("Subtract", 3) # Subtract(a, b, result)
Equals = Predicate("Equals", 2)   # Equals(a, b)
LessThan = Predicate("LessThan", 2) # LessThan(a, b)

V_Arithmetic = Vocabulary(
    name="V_Arithmetic",
    predicates={Add, Subtract, Equals, LessThan}
)

# --- Metaphorical Practice: Arithmetic as Object Collection ---

# This practice is not defined by its own intrinsic rules, but by projecting
# the rules from the P_ObjectManipulation practice.

# Define the mappings for the Pragmatic Projection
# This is the core of the "Arithmetic is Object Collection" metaphor.
metaphor_mappings = {
    # Object Manipulation Predicate -> Arithmetic Predicate
    "Combine": "Add",
    "Remainder": "Subtract",
    "PartOf": "LessThan",
    "IsCollection": "IsNumber" # We can imagine a predicate IsNumber if needed
}

# The MUR that defines the metaphor
ArithmeticIsObjectCollection = PragmaticProjection(
    P_base=P_ObjectManipulation,
    P_elaborated=None, # The target practice is constructed by the projection
    mappings=metaphor_mappings
)
# For the purpose of our model, we can create a Practice that represents
# the *result* of this projection. The DeonticScorekeeper would use the
# MUR to translate rules.

P_ArithmeticAsObjectCollection = Practice(
    name="P_ArithmeticAsObjectCollection",
    vocabulary=V_Arithmetic,
    # The incompatibilities and inferences are not defined here directly.
    # They are *inherited* from P_ObjectManipulation via the metaphor.
    # For example, the `remainder_constraint` from the source practice:
    # Incompatible(Remainder(c2, c1, c3), PartOf(c2, c1))
    # becomes:
    # Incompatible(Subtract(y, x, z), LessThan(y, x))
    # which is the rule that you cannot subtract a larger number from a smaller one.
    incompatibilities=set(), # Populated by the projection mechanism
    inferences=set()         # Populated by the projection mechanism
)

# We update the MUR to point to this new practice
ArithmeticIsObjectCollection.P_elaborated = P_ArithmeticAsObjectCollection


if __name__ == '__main__':
    print("--- Defining the 'Arithmetic is Object Collection' Metaphor ---")
    print(f"\nSource Practice: {ArithmeticIsObjectCollection.P_base.name}")
    print(f"Source Vocabulary: {ArithmeticIsObjectCollection.P_base.vocabulary.name}")
    print(f"\nTarget Practice: {ArithmeticIsObjectCollection.P_elaborated.name}")
    print(f"Target Vocabulary: {ArithmeticIsObjectCollection.P_elaborated.vocabulary.name}")
    print("\nMetaphorical Mappings:")
    for source, target in ArithmeticIsObjectCollection.mappings.items():
        print(f"  - {source} -> {target}")

    print("\nExample of Projected Incoherence:")
    print("Source Incoherence (from P_ObjectManipulation):")
    print("  - You can't take a whole from its part.")
    print("  - Incompatible(Remainder(part, whole, _), PartOf(part, whole))")
    print("\nProjected Incoherence (in P_ArithmeticAsObjectCollection):")
    print("  - You can't subtract a larger number from a smaller one.")
    print("  - Incompatible(Subtract(smaller, larger, _), LessThan(smaller, larger))")
    print("\nThis demonstrates how the logical constraints of the source domain are inherited by the target domain via the metaphor.")
