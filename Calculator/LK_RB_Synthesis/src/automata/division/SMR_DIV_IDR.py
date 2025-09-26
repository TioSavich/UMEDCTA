import sys
import os
from typing import Dict

# Add project root to path to allow importing from src
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../../..')))

from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata

class InverseDistributiveReasoningDivisionAutomaton(BaseAutomaton):
    @property
    def metadata(self) -> StrategyMetadata:
        return StrategyMetadata(
            strategy_id="SMR_DIV_IDR",
            strategy_name="Inverse Distributive Reasoning (Division)",
            description="Decompose T into known multiples of S: T = sum(m_i * S); quotient = sum(m_i).",
            metaphors=[],
            inferences=[],
            visualization_hints=[]
        )

    def __init__(self, inputs: Dict, Base=10):
        super().__init__(inputs, Base)
        self.registers.update({
            'T': self.inputs.get('T', 42),  # Dividend
            'S': self.inputs.get('S', 6),   # Divisor
            'Quotient': 0,
            'CurrentSum': 0,
            'Multiples': []
        })

    def execute_q_start(self):
        self._record_history(f"Starting inverse distributive division: {self.registers['T']} ÷ {self.registers['S']}", highlight=True)
        self.transition('q_init')

    def execute_q_init(self):
        """Initialize division process."""
        self.registers['CurrentSum'] = 0
        self.registers['Quotient'] = 0
        self.registers['Multiples'] = []
        self._record_history(f"Initialized: T={self.registers['T']}, S={self.registers['S']}")
        self.transition('q_search')

    def execute_q_search(self):
        """Search for multiples that sum to T."""
        t = self.registers['T']
        s = self.registers['S']
        current_sum = self.registers['CurrentSum']

        if current_sum >= t:
            self.transition('q_apply')
            return

        # Find next multiple
        next_multiple = ((current_sum // s) + 1) * s
        if next_multiple <= t:
            self.registers['Multiples'].append(next_multiple)
            self.registers['CurrentSum'] = next_multiple
            self._record_history(f"Found multiple: {next_multiple}")
        else:
            self.transition('q_apply')

    def execute_q_apply(self):
        """Apply the distributive reasoning."""
        multiples = self.registers['Multiples']
        s = self.registers['S']

        quotient = sum(m // s for m in multiples)
        self.registers['Quotient'] = quotient

        self.Result = quotient
        self._record_history(f"Division complete: {self.registers['T']} ÷ {self.registers['S']} = {self.Result}", highlight=True)
        self.transition('q_accept')
