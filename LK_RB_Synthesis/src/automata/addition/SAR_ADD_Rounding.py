import sys
import os

# Add project root to path to allow importing from src
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../../..')))

from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata

class RoundingAndAdjustingAdditionAutomaton(BaseAutomaton):
    _metadata = {
        "Name": "Rounding and Adjusting (Addition)",
        "Description": "Select addend closer to next base: round up A -> A' = A + K, compute A' + B, then adjust back: (A' + B) - K.",
        "Cognitive Steps": ['q_start', 'q_calcK', 'q_add', 'q_adjust'],
        "Examples": []
    }

    @property
    def metadata(self) -> StrategyMetadata:
        return StrategyMetadata(**self._metadata)

    def execute_q_start(self):
        # Initial state logic
        pass

    def execute_q_calcK(self):
        # Logic for calcK
        pass

    def execute_q_add(self):
        # Logic for add
        pass

    def execute_q_adjust(self):
        # Logic for adjust
        pass
