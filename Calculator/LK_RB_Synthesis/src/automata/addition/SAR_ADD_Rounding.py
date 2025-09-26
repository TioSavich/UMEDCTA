import sys
import os
from typing import Dict

# Add project root to path to allow importing from src
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../../..')))

from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata

class RoundingAndAdjustingAdditionAutomaton(BaseAutomaton):
    @property
    def metadata(self) -> StrategyMetadata:
        return StrategyMetadata(
            strategy_id="SAR_ADD_Rounding",
            strategy_name="Rounding and Adjusting (Addition)",
            description="Select addend closer to next base: round up A -> A' = A + K, compute A' + B, then adjust back: (A' + B) - K.",
            metaphors=[],
            inferences=[],
            visualization_hints=[]
        )

    def __init__(self, inputs: Dict, Base=10):
        super().__init__(inputs, Base)
        self.registers.update({
            'A': self.inputs.get('A', 8),
            'B': self.inputs.get('B', 7),
            'K': 0,
            'A_rounded': 0,
            'Sum': 0,
            'Result': 0
        })

    def execute_q_start(self):
        self._record_history(f"Starting rounding addition: {self.registers['A']} + {self.registers['B']}", highlight=True)
        self.transition('q_calcK')

    def execute_q_calcK(self):
        """Calculate K needed to round A to next base."""
        a = self.registers['A']
        base = self.Base

        if a % base == 0:
            k = 0
        else:
            k = base - (a % base)

        self.registers['K'] = k
        self.registers['A_rounded'] = a + k

        self._record_history(f"Calculated K={k} to round {a} to {a + k}")
        self.transition('q_add')

    def execute_q_add(self):
        """Add the rounded A and B."""
        a_rounded = self.registers['A_rounded']
        b = self.registers['B']
        sum_result = a_rounded + b

        self.registers['Sum'] = sum_result
        self._record_history(f"Added: {a_rounded} + {b} = {sum_result}")
        self.transition('q_adjust')

    def execute_q_adjust(self):
        """Adjust back by subtracting K."""
        sum_result = self.registers['Sum']
        k = self.registers['K']
        result = sum_result - k

        self.registers['Result'] = result
        self.Result = result

        self._record_history(f"Adjusted: {sum_result} - {k} = {result}", highlight=True)
        self.transition('q_accept')
