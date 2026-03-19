import sys
import os
from typing import Dict

# Add project root to path to allow importing from src
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../../..')))

from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata

class DistributiveReasoningMultiplicationAutomaton(BaseAutomaton):
    @property
    def metadata(self) -> StrategyMetadata:
        return StrategyMetadata(
            strategy_id="SMR_MULT_DR",
            strategy_name="Distributive Reasoning (Multiplication)",
            description="Decompose S = S1 + S2, compute N*S1 and N*S2, then sum.",
            metaphors=[],
            inferences=[],
            visualization_hints=[]
        )

    def __init__(self, inputs: Dict, Base=10):
        super().__init__(inputs, Base)
        self.registers.update({
            'N': self.inputs.get('N', 5),   # Multiplicand
            'S': self.inputs.get('S', 7),   # Multiplier
            'S1': 0,
            'S2': 0,
            'P1': 0,
            'P2': 0,
            'Total': 0
        })

    def execute_q_start(self):
        self._record_history(f"Starting distributive reasoning: {self.registers['N']} × {self.registers['S']}", highlight=True)
        self.transition('q_split')

    def execute_q_split(self):
        """Split S into S1 + S2."""
        s = self.registers['S']
        # Simple split: S1 = S//2, S2 = S - S1
        s1 = s // 2
        s2 = s - s1

        self.registers['S1'] = s1
        self.registers['S2'] = s2

        self._record_history(f"Split {s} into {s1} + {s2}")
        self.transition('q_P1')

    def execute_q_P1(self):
        """Compute N × S1."""
        n = self.registers['N']
        s1 = self.registers['S1']
        p1 = n * s1

        self.registers['P1'] = p1
        self._record_history(f"Computed {n} × {s1} = {p1}")
        self.transition('q_P2')

    def execute_q_P2(self):
        """Compute N × S2."""
        n = self.registers['N']
        s2 = self.registers['S2']
        p2 = n * s2

        self.registers['P2'] = p2
        self._record_history(f"Computed {n} × {s2} = {p2}")
        self.transition('q_sum')

    def execute_q_sum(self):
        """Sum the partial products."""
        p1 = self.registers['P1']
        p2 = self.registers['P2']
        total = p1 + p2

        self.registers['Total'] = total
        self.Result = total

        self._record_history(f"Sum: {p1} + {p2} = {total}", highlight=True)
        self.transition('q_accept')
