import sys
import os
from typing import Dict

# Add project root to path to allow importing from src
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../../..')))

from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata

class DealingByOnesDivisionSharingAutomaton(BaseAutomaton):
    @property
    def metadata(self) -> StrategyMetadata:
        return StrategyMetadata(
            strategy_id="SMR_DIV_DealingByOnes",
            strategy_name="Dealing by Ones (Division - Sharing)",
            description="Distribute single units round-robin into N groups until total T exhausted.",
            metaphors=[],
            inferences=[],
            visualization_hints=[]
        )

    def __init__(self, inputs: Dict, Base=10):
        super().__init__(inputs, Base)
        self.registers.update({
            'T': self.inputs.get('T', 12),  # Total items to distribute
            'N': self.inputs.get('N', 3),   # Number of groups
            'CurrentItem': 0,
            'Groups': [],
            'Quotient': 0
        })

    def execute_q_start(self):
        self._record_history(f"Starting dealing by ones: {self.registers['T']} items into {self.registers['N']} groups", highlight=True)
        self.transition('q_init')

    def execute_q_init(self):
        """Initialize groups."""
        n = self.registers['N']
        self.registers['Groups'] = [0] * n
        self.registers['CurrentItem'] = 0
        self._record_history(f"Initialized {n} empty groups")
        self.transition('q_deal')

    def execute_q_deal(self):
        """Deal items one by one to groups."""
        t = self.registers['T']
        n = self.registers['N']
        current_item = self.registers['CurrentItem']
        groups = self.registers['Groups']

        if current_item >= t:
            # All items dealt
            self.registers['Quotient'] = groups[0]  # All groups should have same count
            self.Result = self.registers['Quotient']
            self._record_history(f"Dealing complete: {t} ÷ {n} = {self.Result}", highlight=True)
            self.transition('q_accept')
            return

        # Deal current item to next group
        group_index = current_item % n
        groups[group_index] += 1
        self._record_history(f"Dealt item {current_item + 1} to group {group_index + 1}")

        self.registers['CurrentItem'] = current_item + 1
