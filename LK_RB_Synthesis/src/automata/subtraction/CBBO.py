import sys
import os
import json
import pandas as pd

# Add project root to path to allow importing from src
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../..')))

from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata, EmbodiedMetaphor, MaterialInference
from typing import Dict

class CBBOAutomaton(BaseAutomaton):
    """
    CBBO (Counting Back): Start at M, subtract S iteratively. Result is final position.
    This models a 'take away' approach where the subtrahend is removed from the minuend
    in chunks of bases and ones.
    """
    @property
    def metadata(self) -> StrategyMetadata:
        return StrategyMetadata(
            strategy_name="CBBO (Counting Back - Take Away)",
            description=(
                "Starts at the minuend (M) and counts back by the amount of the subtrahend (S), "
                "first by bases and then by ones. The result is the final value after taking away S."
            ),
            metaphors=[
                EmbodiedMetaphor(
                    name="Arithmetic as Object Manipulation",
                    source_domain="Object Manipulation",
                    target_domain="Arithmetic",
                    entailments="Taking objects away from a collection reduces its size. The final size is the result."
                )
            ],
            inferences=[
                MaterialInference(
                    name="Place Value Decomposition",
                    premise="A number S can be decomposed into its place value parts (e.g., 65 = six 10s and five 1s).",
                    conclusion="One can subtract S by sequentially taking away its component parts.",
                    prerequisites=["Counting skills", "Understanding of base-10 system"]
                )
            ],
            visualization_hints=["Blocks", "NumberLine"]
        )

    def __init__(self, inputs: Dict, Base=10):
        super().__init__(inputs, Base)
        self.registers.update({
            'M': self.inputs.get('M', 0),
            'S': self.inputs.get('S', 0),
            'CurrentValue': 0,
            'BaseCounter': 0,
            'OneCounter': 0
        })
        if self.registers['S'] > self.registers['M']:
            self.state = 'q_error'
            self._record_history(f"Error: Subtrahend ({self.registers['S']}) > Minuend ({self.registers['M']}).")

    def execute_q_start(self):
        self._record_history("Start.", highlight=True)
        self.transition('q_init')

    def execute_q_init(self):
        self.registers['CurrentValue'] = self.registers['M']
        self.registers['BaseCounter'] = self.registers['S'] // self.Base
        self.registers['OneCounter'] = self.registers['S'] % self.Base
        self._record_history(f"Initialize at M ({self.registers['M']}). Decompose S ({self.registers['S']}): {self.registers['BaseCounter']} bases, {self.registers['OneCounter']} ones.")
        self.transition('q_sub_bases')

    def execute_q_sub_bases(self):
        """Iteratively subtract bases."""
        if self.registers['BaseCounter'] > 0:
            self.registers['CurrentValue'] -= self.Base
            self.registers['BaseCounter'] -= 1
            self._record_history(f"Count back by base (-{self.Base}). New Value={self.registers['CurrentValue']}.")
        else:
            self._record_history("Bases finished. Switching to ones.", highlight=True)
            self.transition('q_sub_ones')

    def execute_q_sub_ones(self):
        """Iteratively subtract ones."""
        if self.registers['OneCounter'] > 0:
            self.registers['CurrentValue'] -= 1
            self.registers['OneCounter'] -= 1
            self._record_history(f"Count back by one (-1). New Value={self.registers['CurrentValue']}.")
        else:
            self.Result = self.registers['CurrentValue']
            self._record_history(f"Subtraction finished. Result (Final Position) = {self.Result}.", highlight=True)
            self.transition('q_accept')

if __name__ == '__main__':
    # Test Case: 94 - 65
    M_test = 94
    S_test = 65
    print(f"=== Testing CBBO Strategy: {M_test} - {S_test} ===")
    cbbo = CBBOAutomaton(inputs={'M': M_test, 'S': S_test})
    result = cbbo.run()
    print(f"Final Result: {result}")
    cbbo.display_history()
    print("\n--- JSON Export ---")
    print(cbbo.export_trace_json())
