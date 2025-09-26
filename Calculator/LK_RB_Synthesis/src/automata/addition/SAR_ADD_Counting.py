import sys
import os
from typing import Dict

# Add project root to path to allow importing from src
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../../..')))

from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata

class CountingAndCountingOnAutomaton(BaseAutomaton):
    @property
    def metadata(self) -> StrategyMetadata:
        return StrategyMetadata(
            strategy_id="SAR_ADD_Counting",
            strategy_name="Counting and Counting On",
            description="Sequential unit counting within a bounded base-10 place-value structure.",
            metaphors=[],
            inferences=[],
            visualization_hints=[]
        )

    def __init__(self, inputs: Dict, Base=10):
        super().__init__(inputs, Base)
        self.registers.update({
            'A': self.inputs.get('A', 8),
            'B': self.inputs.get('B', 5),
            'Current': 0,
            'TensCount': 0,
            'HundredsCount': 0,
            'Result': 0
        })

    def execute_q_start(self):
        self._record_history(f"Starting counting addition: {self.registers['A']} + {self.registers['B']}", highlight=True)
        self.transition('q_idle')

    def execute_q_idle(self):
        """Initialize counting from A."""
        self.registers['Current'] = self.registers['A']
        self._record_history(f"Starting count from {self.registers['A']}")
        self.transition('q_inc_tens')

    def execute_q_inc_tens(self):
        """Increment by tens."""
        b = self.registers['B']
        tens_needed = b // 10
        current_tens = self.registers['TensCount']

        if current_tens < tens_needed:
            self.registers['Current'] += 10
            self.registers['TensCount'] += 1
            self._record_history(f"Added ten: {self.registers['Current']}")
            return  # Stay in this state

        self._record_history(f"Added {tens_needed} tens")
        self.transition('q_inc_hundreds')

    def execute_q_inc_hundreds(self):
        """Increment by hundreds."""
        b = self.registers['B']
        hundreds_needed = b // 100
        current_hundreds = self.registers['HundredsCount']

        if current_hundreds < hundreds_needed:
            self.registers['Current'] += 100
            self.registers['HundredsCount'] += 1
            self._record_history(f"Added hundred: {self.registers['Current']}")
            return  # Stay in this state

        self._record_history(f"Added {hundreds_needed} hundreds")
        self.transition('q_halt')

    def execute_q_halt(self):
        """Finalize the result."""
        self.registers['Result'] = self.registers['Current']
        self.Result = self.registers['Result']
        self._record_history(f"Addition complete: {self.registers['A']} + {self.registers['B']} = {self.Result}", highlight=True)
        self.transition('q_accept')
