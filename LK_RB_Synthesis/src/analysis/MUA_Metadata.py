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
class StrategyMetadata:
    """Metadata container for analyzing a specific strategy."""
    strategy_name: str
    description: str = ""
    metaphors: List[EmbodiedMetaphor] = field(default_factory=list)
    inferences: List[MaterialInference] = field(default_factory=list)
    visualization_hints: List[str] = field(default_factory=list) # e.g., ["NumberLine", "Blocks"]
