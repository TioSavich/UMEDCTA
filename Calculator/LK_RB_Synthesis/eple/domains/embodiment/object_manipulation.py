
from eple.core.logic_terms import Var, Predicate, Incompatibility, Inference
from eple.core.mua import Vocabulary, Practice

# --- Vocabulary for Object Manipulation ---

# Predicates
IsCollection = Predicate("IsCollection", 1)  # IsCollection(c)
PartOf = Predicate("PartOf", 2)              # PartOf(sub_collection, super_collection)
Combine = Predicate("Combine", 3)            # Combine(c1, c2, combined_c) -> c1 + c2 = combined_c
Remainder = Predicate("Remainder", 3)        # Remainder(c1, c2, remainder_c) -> c1 - c2 = remainder_c

# Vocabulary
V_ObjectManipulation = Vocabulary(
    name="V_ObjectManipulation",
    predicates={IsCollection, PartOf, Combine, Remainder}
)

# --- Practice of Object Manipulation ---

# Terms
c1 = Var("c1")
c2 = Var("c2")
c3 = Var("c3")

# Inference Rules
# 1. Transitivity of PartOf: If c1 is part of c2, and c2 is part of c3, then c1 is part of c3.
transitivity_of_partof = Inference(
    antecedent=[PartOf(c1, c2), PartOf(c2, c3)],
    consequent=PartOf(c1, c3)
)

# 2. Definition of Combine: If you combine c1 and c2 to get c3, then c1 and c2 are parts of c3.
combine_implies_parts = Inference(
    antecedent=[Combine(c1, c2, c3)],
    consequent=[PartOf(c1, c3), PartOf(c2, c3)]
)

# 3. Definition of Remainder: If c3 is the remainder of taking c2 from c1, then c3 is part of c1.
remainder_implies_part = Inference(
    antecedent=[Remainder(c1, c2, c3)],
    consequent=PartOf(c3, c1)
)

# Incompatibilities
# 1. Asymmetry of PartOf: A collection cannot be part of its own proper part.
#    If c1 is part of c2, then c2 cannot be part of c1 (unless they are identical, which we ignore for simplicity).
#    We model this as: PartOf(c1, c2) is incompatible with PartOf(c2, c1).
asymmetry_of_partof = Incompatibility(
    p1=PartOf(c1, c2),
    p2=PartOf(c2, c1)
)

# 2. Constraint on Remainder: You cannot take a collection from one of its own parts.
#    If c2 is part of c1, you cannot take c1 from c2.
#    Remainder(c2, c1, c3) is incompatible with PartOf(c2, c1).
remainder_constraint = Incompatibility(
    p1=Remainder(c2, c1, c3),
    p2=PartOf(c2, c1)
)

# The Practice
P_ObjectManipulation = Practice(
    name="P_ObjectManipulation",
    vocabulary=V_ObjectManipulation,
    incompatibilities={
        asymmetry_of_partof,
        remainder_constraint
    },
    inferences={
        transitivity_of_partof,
        combine_implies_parts,
        remainder_implies_part
    }
)

if __name__ == '__main__':
    print(f"Practice: {P_ObjectManipulation.name}")
    print(f"Vocabulary: {P_ObjectManipulation.vocabulary.name}")
    print("\nPredicates:")
    for pred in P_ObjectManipulation.vocabulary.predicates:
        print(f"  - {pred}")

    print("\nIncompatibilities:")
    for incomp in P_ObjectManipulation.incompatibilities:
        print(f"  - {incomp}")

    print("\nInferences:")
    for infer in P_ObjectManipulation.inferences:
        print(f"  - {infer}")

    # Example Scenarios
    coll_A = Var("coll_A")
    coll_B = Var("coll_B")
    coll_C = Var("coll_C")

    # Scenario 1: Transitivity
    fact1 = PartOf(coll_A, coll_B)
    fact2 = PartOf(coll_B, coll_C)
    print(f"\nGiven: {fact1} and {fact2}")
    print(f"The engine can infer: {PartOf(coll_A, coll_C)}")

    # Scenario 2: Incoherence
    fact3 = PartOf(coll_A, coll_B)
    fact4 = PartOf(coll_B, coll_A)
    print(f"\nGiven: {fact3} and {fact4}")
    print("The engine would find this incoherent due to the asymmetry of PartOf.")

    # Scenario 3: Remainder Constraint
    fact5 = PartOf(coll_A, coll_B) # A is part of B
    fact6 = Remainder(coll_A, coll_B, coll_C) # Trying to take B from A
    print(f"\nGiven: {fact5} and {fact6}")
    print("The engine would find this incoherent, as you can't take a whole from its part.")
