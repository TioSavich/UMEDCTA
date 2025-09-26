# src/analysis/MUA_Metadata.py
from dataclasses import dataclass, field
from typing import List, Dict, Optional

@dataclass
class EmbodiedMetaphor:
    """Describes the Lakoff/Núñez conceptual metaphors involved (WMCF)."""
    name: str # e.g., "Arithmetic as Motion Along a Path"
    source_domain: str
    target_domain: str
    entailments: str # Key entailments relevant to the strategy

@dataclass
class MaterialInference:
    """Describes the key Brandomian material inferences enacted (BSD)."""
    name: str # e.g., "Invariance of Distance under Translation"
    premise: str
    conclusion: str
    # Practices required to competently make this inference (PP-Necessities)
    prerequisites: List[str] = field(default_factory=list)

@dataclass
class Practice:
    """Describes a specific cognitive or physical practice."""
    id: str # Short ID, e.g., "P_StableOrder" or "P_IdentifyK"
    description: str

@dataclass
class LXRelation:
    """Describes an LX relationship where this strategy (V') elaborates another (V)."""
    elaborates_strategy_id: str # ID of the strategy being elaborated
    implicit_practice: str # What the base strategy 'does'
    explicit_principle: str # What this strategy allows one to 'say'
    explanation: str = ""

@dataclass
class StrategyMetadata:
    """Metadata container for analyzing a specific strategy."""
    strategy_id: str
    strategy_name: str
    description: str = ""
    metaphors: List[EmbodiedMetaphor] = field(default_factory=list)
    visualization_hints: List[str] = field(default_factory=list) # e.g., ["NumberLine", "Blocks"]
    
    # The key concepts introduced (e.g., "Invariance of Distance")
    deployed_vocabulary: str = ""
    inferences: List[MaterialInference] = field(default_factory=list)

    # --- Brandomian Analysis Fields ---

    # Prerequisite Practices (PP-Necessities)
    pp_necessities: List[Practice] = field(default_factory=list)

    # Algorithmic Elaboration (PP-Sufficiencies)
    pp_sufficiencies_alg_elaboration: List[Practice] = field(default_factory=list)

    # LX Relations (VV-Resultance / Expressive Bootstrapping)
    lx_relations: List[LXRelation] = field(default_factory=list)
