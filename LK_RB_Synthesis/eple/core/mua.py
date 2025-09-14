# -*- coding: utf-8 -*-
"""
This file implements Brandom's Meaning-Use Analysis (MUA) framework.
It defines the core concepts of Vocabulary, Practice, and the relations
that connect them (Meaning-Use Relations or MURs).
"""

class Vocabulary:
    """
    Represents a 'Vocabulary' (V) in the MUA framework. This is the "Saying"
    aspect, consisting of a set of predicates.
    """
    def __init__(self, name, predicates=None):
        self.name = name
        self.predicates = predicates if predicates is not None else set()

    def __repr__(self):
        return f"Vocabulary(name='{self.name}')"

class Practice:
    """
    Represents a 'Practice' (P) in the MUA framework. This is the "Doing"
    aspect, defined by a set of rules (incompatibilities and inferences).
    """
    def __init__(self, name, vocabulary=None, incompatibilities=None, inferences=None):
        self.name = name
        self.vocabulary = vocabulary
        self.incompatibilities = incompatibilities if incompatibilities is not None else set()
        self.inferences = inferences if inferences is not None else set()
    
    def __repr__(self):
        return f"Practice(name='{self.name}')"


# =================================================================
# Part 2: Meaning-Use Relations (MURs)
# =================================================================

class MeaningUseRelation:
    """Base class for all Meaning-Use Relations."""
    def __repr__(self):
        return f"{self.__class__.__name__}"

class PP_Sufficiency(MeaningUseRelation):
    """
    Represents that one practice (P_elaborated) is sufficient to perform
    another practice (P_base). This is the core of elaboration.
    """
    def __init__(self, P_base, P_elaborated):
        self.P_base = P_base
        self.P_elaborated = P_elaborated

    def __repr__(self):
        return f"{self.__class__.__name__}(P_base='{self.P_base.name}', P_elaborated='{self.P_elaborated.name}')"


# --- Mechanisms for PP-Sufficiency ---
# These are specific types of PP-Sufficiency

class AlgorithmicElaboration(PP_Sufficiency):
    """
    Represents a deterministic, rule-based transformation between practices.
    This corresponds to learning a new procedure based on existing ones.
    It is a form of PP-Sufficiency.
    """
    def __init__(self, P_base, P_elaborated):
        super().__init__(P_base, P_elaborated)

class PragmaticProjection(PP_Sufficiency):
    """
    Represents a non-algorithmic, metaphorical, or abductive leap between
    practices. This is where new conceptual structures are projected onto new
    domains. It is a form of PP-Sufficiency.
    """
    def __init__(self, P_base, P_elaborated, mappings=None):
        super().__init__(P_base, P_elaborated)
        self.mappings = mappings if mappings is not None else {}


# =================================================================
# Part 3: MUA Analysis Functions
# =================================================================

def find_pragmatic_metavocabulary(v_target, p_context, all_practices, all_murs):
    """
    Finds vocabularies that serve as a pragmatic metavocabulary for a target vocabulary.
    V_meta is a pragmatic metavocabulary for V_target if:
    1. There exists a practice P_elaborated that elaborates P_context (the practice for V_target).
       (i.e., there is a PP-Sufficiency from P_context to P_elaborated)
    2. V_meta is VP-sufficient for P_elaborated. In our model, this means V_meta is
       the vocabulary associated with P_elaborated.
    """
    metavocabularies = set()
    
    # Find all practices that elaborate the context practice
    elaborating_practices = {
        mur.P_elaborated for mur in all_murs
        if isinstance(mur, PP_Sufficiency) and mur.P_base == p_context
    }
    
    if not elaborating_practices:
        return set()
        
    # For each elaborating practice, get its associated vocabulary.
    for p_elab in elaborating_practices:
        if p_elab.vocabulary:
             metavocabularies.add(p_elab.vocabulary)
                
    return metavocabularies

def is_LX(p_base, p_elaborated, v_base, v_elaborated, all_practices, all_murs):
    """
    Checks if the pair (P2, V2) is an Elaborated-Explicating (LX) pair for (P1, V1).
    (p_elaborated, v_elaborated) is LX for (p_base, v_base) if:
    1. p_elaborated is an elaboration of p_base.
       (There is a PP-Sufficiency from p_base to p_elaborated).
    2. v_elaborated makes explicit the implicit structure of p_base.
       (v_elaborated is a pragmatic metavocabulary for v_base in the context of p_base).
    """
    # Condition 1: Is p_elaborated an elaboration of p_base?
    is_elaboration = any(
        isinstance(mur, PP_Sufficiency) and
        mur.P_base == p_base and
        mur.P_elaborated == p_elaborated
        for mur in all_murs
    )

    if not is_elaboration:
        print("DEBUG: is_LX failed at Condition 1 (is_elaboration)")
        return False

    # Condition 2: Is v_elaborated a pragmatic metavocabulary for v_base?
    pragmatic_metavocabularies = find_pragmatic_metavocabulary(
        v_target=v_base,
        p_context=p_base,
        all_practices=all_practices,
        all_murs=all_murs
    )
    
    is_explicative = v_elaborated in pragmatic_metavocabularies
    
    if not is_explicative:
        print("DEBUG: is_LX failed at Condition 2 (is_explicative)")
        print(f"  - Target pragmatic metavocabularies: {[v.name for v in pragmatic_metavocabularies]}")
        print(f"  - Tested vocabulary: {v_elaborated.name}")
        return False

    return True
