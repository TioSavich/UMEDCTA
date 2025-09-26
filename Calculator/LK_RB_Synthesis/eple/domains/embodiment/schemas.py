
from eple.core.logic_terms import Term, Predicate, Incompatibility, Inference
from eple.core.mua import Vocabulary, Practice

# --- Vocabulary for Container Schema ---

# Predicates
Inside = Predicate("Inside", 2)  # Inside(object, container)
Outside = Predicate("Outside", 2) # Outside(object, container)

# Vocabulary
V_Container = Vocabulary(
    name="V_Container",
    predicates={Inside, Outside}
)

# --- Practice of Container Schema ---

# Incompatibilities
# An object cannot be both inside and outside the same container.
in_out_incompatibility = Incompatibility(
    p1=Inside(Term("x"), Term("y")),
    p2=Outside(Term("x"), Term("y"))
)

# Inference Rules
# If object x is inside container y, and y is inside container z, then x is inside z.
transitivity_of_inside = Inference(
    antecedent=[Inside(Term("x"), Term("y")), Inside(Term("y"), Term("z"))],
    consequent=Inside(Term("x"), Term("z"))
)

# If an object is not inside a container, it must be outside it.
# This is a strong assumption and could be debated, but we'll include it for now.
# This is represented by an incompatibility: not being inside is incompatible with not being outside.
neg_in_neg_out_incompatibility = Incompatibility(
    p1=Incompatibility.neg(Inside(Term("x"), Term("y"))),
    p2=Incompatibility.neg(Outside(Term("x"), Term("y")))
)


# The Practice
P_ContainerSchema = Practice(
    name="P_ContainerSchema",
    vocabulary=V_Container,
    incompatibilities={
        in_out_incompatibility,
        neg_in_neg_out_incompatibility
    },
    inferences={
        transitivity_of_inside
    }
)

if __name__ == '__main__':
    # Example of how to access the components of the practice
    print(f"Practice: {P_ContainerSchema.name}")
    print(f"Vocabulary: {P_ContainerSchema.vocabulary.name}")
    print("Predicates:")
    for pred in P_ContainerSchema.vocabulary.predicates:
        print(f"  - {pred}")

    print("\nIncompatibilities:")
    for incomp in P_ContainerSchema.incompatibilities:
        print(f"  - {incomp}")

    print("\nInferences:")
    for infer in P_ContainerSchema.inferences:
        print(f"  - {infer}")

    # Example instantiation
    obj_a = Term("a")
    cont_b = Term("b")
    cont_c = Term("c")

    fact1 = Inside(obj_a, cont_b)
    fact2 = Inside(cont_b, cont_c)

    print(f"\nGiven: {fact1} and {fact2}")
    print(f"Can we infer: {Inside(obj_a, cont_c)}?")
    # In a real scenario, the DeonticScorekeeper would use the `transitivity_of_inside`
    # rule to make this inference.
    print("This would be handled by the DeonticScorekeeper engine.")

    fact3 = Inside(obj_a, cont_b)
    fact4 = Outside(obj_a, cont_b)
    print(f"\nAre '{fact3}' and '{fact4}' compatible?")
    print("The DeonticScorekeeper would identify this as an incoherence based on `in_out_incompatibility`.")
