import sys
import os
from typing import Dict

# Add project root to path to allow importing from src
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../../..')))

from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata

class UsingCommutativeReasoningDivisionAutomaton(BaseAutomaton):
    @property
    def metadata(self) -> StrategyMetadata:
        return StrategyMetadata(
            strategy_id="SMR_DIV_UCR",
            strategy_name="Using Commutative Reasoning (Division)",
            description="For E / G: iteratively accumulate G until total E reached; iteration count is quotient.",
            metaphors=[],
            inferences=[],
            visualization_hints=[]
        )

    def __init__(self, inputs: Dict, Base=10):
        super().__init__(inputs, Base)
        self.registers.update({
            'E': self.inputs.get('E', 18),  # Dividend
            'G': self.inputs.get('G', 3),   # Divisor
            'CurrentSum': 0,
            'Count': 0,
            'Quotient': 0
        })

    def execute_q_start(self):
        self._record_history(f"Starting commutative reasoning division: {self.registers['E']} ÷ {self.registers['G']}", highlight=True)
        self.transition('q_init')

    def execute_q_init(self):
        """Initialize division process."""
        self.registers['CurrentSum'] = 0
        self.registers['Count'] = 0
        self._record_history(f"Initialized: E={self.registers['E']}, G={self.registers['G']}")
        self.transition('q_iterate')

    def execute_q_iterate(self):
        """Iteratively accumulate G until reaching E."""
        e = self.registers['E']
        g = self.registers['G']
        current_sum = self.registers['CurrentSum']
        count = self.registers['Count']

        if current_sum >= e:
            self.transition('q_check')
            return

        # Add another G
        self.registers['CurrentSum'] = current_sum + g
        self.registers['Count'] = count + 1
        self._record_history(f"Iteration {count + 1}: {current_sum} + {g} = {current_sum + g}")

    def execute_q_check(self):
        """Check if we've reached exactly E."""
        e = self.registers['E']
        current_sum = self.registers['CurrentSum']
        count = self.registers['Count']

        if current_sum == e:
            self.registers['Quotient'] = count
            self.Result = count
            self._record_history(f"Division complete: {e} ÷ {self.registers['G']} = {self.Result}", highlight=True)
            self.transition('q_accept')
        else:
            self.Result = f"Cannot divide {e} by {self.registers['G']} evenly"
            self._record_history(f"Division incomplete: remainder = {e - (current_sum - self.registers['G'])}", highlight=True)
            self.transition('q_accept')
