import sys
import os
from typing import Dict

# Add project root to path to allow importing from src
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../../..')))

from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata

class CoordinatingTwoCountsC2CAutomaton(BaseAutomaton):
    @property
    def metadata(self) -> StrategyMetadata:
        return StrategyMetadata(
            strategy_id="SMR_MULT_C2C",
            strategy_name="Coordinating Two Counts (C2C)",
            description="Nested counting: items within group, groups within total.",
            metaphors=[],
            inferences=[],
            visualization_hints=[]
        )

    def __init__(self, inputs: Dict, Base=10):
        super().__init__(inputs, Base)
        self.registers.update({
            'N': self.inputs.get('N', 3),   # Number of groups
            'M': self.inputs.get('M', 4),   # Items per group
            'CurrentGroup': 0,
            'CurrentItem': 0,
            'Total': 0
        })

    def execute_q_start(self):
        self._record_history(f"Starting C2C multiplication: {self.registers['N']} groups × {self.registers['M']} items each", highlight=True)
        self.transition('q_init')

    def execute_q_init(self):
        """Initialize counting process."""
        self.registers['CurrentGroup'] = 0
        self.registers['CurrentItem'] = 0
        self.registers['Total'] = 0
        self._record_history("Initialized counting process")
        self.transition('q_checkG')

    def execute_q_checkG(self):
        """Check if there are more groups to process."""
        n = self.registers['N']
        current_group = self.registers['CurrentGroup']

        if current_group >= n:
            self.Result = self.registers['Total']
            self._record_history(f"Multiplication complete: {n} × {self.registers['M']} = {self.Result}", highlight=True)
            self.transition('q_accept')
            return

        self._record_history(f"Processing group {current_group + 1} of {n}")
        self.transition('q_countItems')

    def execute_q_countItems(self):
        """Count items in current group."""
        m = self.registers['M']
        current_item = self.registers['CurrentItem']

        if current_item >= m:
            self.registers['CurrentItem'] = 0
            self.registers['CurrentGroup'] += 1
            self._record_history(f"Group {self.registers['CurrentGroup']} complete")
            self.transition('q_checkG')
            return

        self.registers['Total'] += 1
        self.registers['CurrentItem'] += 1
        self._record_history(f"Counted item {current_item + 1} in group {self.registers['CurrentGroup'] + 1}")

    def execute_q_nextGroup(self):
        """Move to next group."""
        self.registers['CurrentGroup'] += 1
        self.registers['CurrentItem'] = 0
        self._record_history(f"Moving to group {self.registers['CurrentGroup'] + 1}")
        self.transition('q_checkG')
