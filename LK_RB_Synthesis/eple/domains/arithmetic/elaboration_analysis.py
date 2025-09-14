import sys
import os

# Add the project root to the Python path to resolve the 'eple' module
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..'))
sys.path.insert(0, project_root)

"""
This module performs a Meaning-Use Analysis (MUA) on the arithmetic
strategies to model their evolutionary relationship.
"""
from eple.core.mua import Vocabulary, Practice, AlgorithmicElaboration, is_LX, find_pragmatic_metavocabulary
from eple.core.logic_terms import Predicate, Term
from eple.domains.arithmetic.strategies import CountingOn, RMB

# --- Define Vocabularies and Practices for the Strategies ---

# 1. CountingOn Strategy
# The vocabulary is simple: it just performs addition.
V_Add = Vocabulary(
    name="V_Add",
    predicates={Predicate("Add", 3)} # Add(a, b, result)
)

# The practice of CountingOn is the execution of the machine. We can represent
# its core capability as a single inference rule.
P_CountingOn = Practice(
    name="P_CountingOn",
    vocabulary=V_Add,
    # The practice provides a way to infer the result of Add(a, b, ?).
    # The implementation is the RegisterMachine itself.
    # For the MUA, we are concerned with what it *does*, not how it does it.
    inferences=set(), # The 'how' is implicit in the machine.
    incompatibilities=set()
)


# 2. RMB Strategy
# The vocabulary is richer, making concepts like 'base' and 'gap' explicit.
V_RMB = Vocabulary(
    name="V_RMB",
    predicates={
        Predicate("Add", 3),
        Predicate("Decompose", 3), # Decompose(n, part1, part2)
        Predicate("RoundUp", 3),   # RoundUp(n, base, result)
        Predicate("GetGap", 3)     # GetGap(n, base, gap)
    }
)

# The practice of RMB makes the sub-procedures explicit.
P_RMB = Practice(
    name="P_RMB",
    vocabulary=V_RMB,
    # The practice of RMB makes explicit the subroutines that were implicit
    # in the simple loop of CountingOn.
    inferences=set(),
    incompatibilities=set()
)

# --- Define the Meaning-Use Relation (MUR) ---

# The RMB strategy is an Algorithmic Elaboration of the CountingOn strategy.
# It takes the implicit process of "getting from a to a+b" and elaborates it
# into a structured algorithm: find a gap, fill it, add the rest.
elaboration = AlgorithmicElaboration(
    P_base=P_CountingOn,
    P_elaborated=P_RMB
)

# --- Perform the Analysis ---

def analyze_strategy_evolution():
    """
    Uses the MUA framework to analyze the relationship between CountingOn and RMB.
    """
    print("--- MUA Analysis: Strategy Evolution ---")

    # We need to define the set of all practices and MURs.
    practices = {P_CountingOn, P_RMB}
    murs = {elaboration}

    # Check if V_RMB is a pragmatic metavocabulary for V_Add relative to P_CountingOn.
    # This means V_RMB can be used to specify the practices that elaborate P_CountingOn.
    pv_meta = find_pragmatic_metavocabulary(V_Add, P_CountingOn, practices, murs)

    print(f"\nBase Practice: {P_CountingOn.name}")
    print(f"Elaborated Practice: {P_RMB.name}")
    print(f"Base Vocabulary: {V_Add.name}")
    print(f"Potential Metavocabulary: {V_RMB.name}")

    is_meta = V_RMB in pv_meta
    print(f"\nIs '{V_RMB.name}' a pragmatic metavocabulary for '{V_Add.name}'? {is_meta}")

    if is_meta:
        print(f"This is because '{V_RMB.name}' contains predicates (like GetGap, Decompose) that specify the steps of the '{P_RMB.name}' practice, which in turn elaborates '{P_CountingOn.name}'.")

    # Check for the LX-relation (Elaborated-Explicating)
    # Does P_RMB elaborate P_CountingOn, and does V_RMB explicate V_Add?
    # The `is_LX` function checks if P2 is an elaboration of P1 and V2 is a pragmatic
    # metavocabulary for V1 in the context of that elaboration.
    is_lx_relation = is_LX(P_CountingOn, P_RMB, V_Add, V_RMB, practices, murs)

    print(f"\nDoes (P_RMB, V_RMB) stand in an LX-relation to (P_CountingOn, V_Add)? {is_lx_relation}")

    if is_lx_relation:
        print("\nConclusion: SUCCESS")
        print(f"The pair ({P_RMB.name}, {V_RMB.name}) is an LX-pair for ({P_CountingOn.name}, {V_Add.name}).")
        print("This formally models the idea that the RMB strategy is a more advanced, 'explicating' version of the basic CountingOn strategy.")
        print("The new vocabulary of RMB ('gap', 'base', etc.) makes explicit the implicit computational structure of the more primitive practice.")
    else:
        print("\nConclusion: FAILED")
        print("The conditions for an LX-relation were not met.")


if __name__ == '__main__':
    analyze_strategy_evolution()
