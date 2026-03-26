import sys
import os
import json
import math
import pandas as pd

# Add project root to path to allow importing from src
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../..')))

from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata, EmbodiedMetaphor, MaterialInference
from typing import Dict

class ChunkingAutomatonA(BaseAutomaton):
    """
    Strategy A: Start at M, subtract chunks of S decomposed by place value.
    """
    @property
    def metadata(self) -> StrategyMetadata:
        return StrategyMetadata(
            strategy_id="ChunkingA_Subtraction",
            strategy_name="Chunking A (Backwards by Part)",
            description="Starts at the minuend (M) and subtracts chunks of the subtrahend (S) based on its place value decomposition.",
            metaphors=[
                EmbodiedMetaphor(
                    name="Arithmetic as Object Manipulation",
                    source_domain="Object Manipulation",
                    target_domain="Arithmetic",
                    entailments="A larger collection can be reduced by taking away smaller collections (parts)."
                )
            ],
            inferences=[
                MaterialInference(
                    name="Place Value Decomposition",
                    premise="A number S can be represented as a sum of its place value components (e.g., 294 = 200 + 90 + 4).",
                    conclusion="One can subtract S by sequentially subtracting its components.",
                    prerequisites=["Understanding of base-10 system"]
                )
            ],
            visualization_hints=["Blocks"]
        )

    def __init__(self, inputs: Dict, Base=10):
        super().__init__(inputs, Base)
        self.registers.update({
            'M': self.inputs.get('M', 0),
            'S': self.inputs.get('S', 0),
            'CurrentValue': 0,
            'S_Remaining': 0,
            'Chunk': 0
        })
        if self.registers['S'] > self.registers['M']:
            self.state = 'q_error'
            self._record_history(f"Error: Subtrahend ({self.registers['S']}) > Minuend ({self.registers['M']}).")

    def execute_q_start(self):
        self._record_history("Start: Initialize.", highlight=True)
        self.transition('q_init')

    def execute_q_init(self):
        self.registers['CurrentValue'] = self.registers['M']
        self.registers['S_Remaining'] = self.registers['S']
        self._record_history(f"Set CurrentValue={self.registers['M']}. S_Remaining={self.registers['S']}.")
        self.transition('q_identify_chunk')

    def execute_q_identify_chunk(self):
        """Identify the next chunk of S by largest place value."""
        if self.registers['S_Remaining'] <= 0:
            self.Result = self.registers['CurrentValue']
            self._record_history(f"S fully subtracted. Result={self.Result}.", highlight=True)
            self.transition('q_accept')
            return

        s_rem = self.registers['S_Remaining']
        if s_rem > 0:
            power = math.floor(math.log(s_rem, self.Base))
            power_value = self.Base**power
            chunk = (s_rem // power_value) * power_value
        else:
            chunk = 0

        self.registers['Chunk'] = chunk
        self._record_history(f"Identified chunk to subtract: {chunk}.", highlight=True)
        self.transition('q_subtract_chunk')

    def execute_q_subtract_chunk(self):
        """Subtract the identified chunk."""
        chunk = self.registers['Chunk']
        self.registers['CurrentValue'] -= chunk
        self.registers['S_Remaining'] -= chunk
        self._record_history(f"Subtracted {chunk}. New Value={self.registers['CurrentValue']}.")
        self.transition('q_identify_chunk')

if __name__ == '__main__':
    # Test Case 1: 400 - 294
    M_test = 400
    S_test = 294
    print(f"=== Test Case: {M_test} - {S_test} ===")
    auto_A = ChunkingAutomatonA(inputs={'M': M_test, 'S': S_test})
    result = auto_A.run()
    print(f"Final Result: {result}")
    auto_A.display_history()
    print("\n--- JSON Export ---")
    print(auto_A.export_trace_json())

    # Test Case 2: 83 - 17
    M_test_2 = 83
    S_test_2 = 17
    print(f"\n=== Test Case: {M_test_2} - {S_test_2} ===")
    auto_A_2 = ChunkingAutomatonA(inputs={'M': M_test_2, 'S': S_test_2})
    result_2 = auto_A_2.run()
    print(f"Final Result: {result_2}")
    auto_A_2.display_history()
