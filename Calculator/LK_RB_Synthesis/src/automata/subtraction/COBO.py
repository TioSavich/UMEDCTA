import sys
import os
import json
import pandas as pd

# Add project root to path to allow importing from src
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../..')))

from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata, EmbodiedMetaphor, MaterialInference
from typing import Dict

class COBOAutomaton(BaseAutomaton):
    """
    COBO (Counting On): Start at S, count up to M iteratively. Result is distance.
    This models a 'missing addend' strategy where the goal is to find the difference
    by counting up from the smaller number (subtrahend) to the larger one (minuend).
    """
    @property
    def metadata(self) -> StrategyMetadata:
        return StrategyMetadata(
            strategy_id="COBO_Subtraction",
            strategy_name="COBO (Counting On - Missing Addend)",
            description=(
                "Starts at the subtrahend (S) and counts up to the minuend (M), first by bases and then by ones. "
                "The result is the total count (distance) added. This is a 'missing addend' approach."
            ),
            metaphors=[
                EmbodiedMetaphor(
                    name="Arithmetic as Motion Along a Path",
                    source_domain="Motion",
                    target_domain="Arithmetic",
                    entailments="The distance between a starting point (S) and an ending point (M) is the length of the path traversed between them."
                )
            ],
            inferences=[
                MaterialInference(
                    name="Iterative Measurement",
                    premise="A distance can be measured by laying a unit of measure end-to-end.",
                    conclusion="The difference between two numbers can be found by repeatedly adding a unit (like 1 or 10) and counting the additions.",
                    prerequisites=["Counting skills", "Understanding of iteration"]
                )
            ],
            visualization_hints=["NumberLine"]
        )

    def __init__(self, inputs: Dict, Base=10):
        super().__init__(inputs, Base)
        self.registers.update({
            'M': self.inputs.get('M', 0),
            'S': self.inputs.get('S', 0),
            'CurrentValue': 0,
            'Distance': 0
        })
        if self.registers['S'] > self.registers['M']:
            self.state = 'q_error'
            self._record_history(f"Error: Subtrahend ({self.registers['S']}) > Minuend ({self.registers['M']}).")

    def execute_q_start(self):
        self._record_history("Start.", highlight=True)
        self.transition('q_init')

    def execute_q_init(self):
        self.registers['CurrentValue'] = self.registers['S']
        self.registers['Distance'] = 0
        self._record_history(f"Initialize at S ({self.registers['S']}). Target is M ({self.registers['M']}).")
        self.transition('q_add_bases')

    def execute_q_add_bases(self):
        """Iteratively add bases, checking for overshoot."""
        if self.registers['CurrentValue'] + self.Base <= self.registers['M']:
            self.registers['CurrentValue'] += self.Base
            self.registers['Distance'] += self.Base
            self._record_history(f"Count on by base (+{self.Base}). New Value={self.registers['CurrentValue']}.")
        else:
            self._record_history("Next base overshoots target. Switching to ones.", highlight=True)
            self.transition('q_add_ones')

    def execute_q_add_ones(self):
        """Iteratively add ones until M is reached."""
        if self.registers['CurrentValue'] < self.registers['M']:
            self.registers['CurrentValue'] += 1
            self.registers['Distance'] += 1
            self._record_history(f"Count on by one (+1). New Value={self.registers['CurrentValue']}.")
        else:
            self.Result = self.registers['Distance']
            self._record_history(f"Target reached. Result (Distance) = {self.Result}.", highlight=True)
            self.transition('q_accept')

if __name__ == '__main__':
    # Test Case: 94 - 65
    M_test = 94
    S_test = 65
    print(f"=== Testing COBO Strategy: {M_test} - {S_test} ===")
    cobo = COBOAutomaton(inputs={'M': M_test, 'S': S_test})
    result = cobo.run()
    print(f"Final Result: {result}")
    cobo.display_history()
    print("\n--- JSON Export ---")
    print(cobo.export_trace_json())
