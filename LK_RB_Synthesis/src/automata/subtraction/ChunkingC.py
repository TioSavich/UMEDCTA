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

class ChunkingAutomatonC(BaseAutomaton):
    """
    Strategy C: Start at M, subtract down to S. Result is the distance traveled.
    Uses strategic subtraction (Reverse RMB logic) modeled iteratively.
    """
    @property
    def metadata(self) -> StrategyMetadata:
        return StrategyMetadata(
            strategy_name="Chunking C (Backwards to Part)",
            description="Starts at the minuend (M) and subtracts in strategic chunks to reach the subtrahend (S). The result is the total distance subtracted. This is another 'missing addend' variant.",
            metaphors=[
                EmbodiedMetaphor(
                    name="Arithmetic as Motion Along a Path",
                    source_domain="Motion",
                    target_domain="Arithmetic",
                    entailments="The distance between two points can be found by starting at the second point and measuring the journey back to the first."
                )
            ],
            inferences=[
                MaterialInference(
                    name="Rounding to Make Bases (RMB) - Inverse",
                    premise="It is easier to subtract numbers when starting from a multiple of the base.",
                    conclusion="One should subtract a small amount (K) to reach a base, then subtract larger chunks.",
                    prerequisites=["Understanding of base-10 system", "Part-whole knowledge"]
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
            'Distance': 0,
            'K': 0,
            'TargetBase': 0,
            'internal_temp': 0,
            'Chunk': 0
        })
        if self.registers['S'] > self.registers['M']:
            self.state = 'q_error'
            self._record_history(f"Error: Subtrahend ({self.registers['S']}) > Minuend ({self.registers['M']}).")

    def transition(self, next_state):
        if next_state == 'q_check_status':
             self.registers['K'] = 0
             self.registers['TargetBase'] = 0
             self.registers['internal_temp'] = 0
        super().transition(next_state)

    def execute_q_start(self):
        self._record_history("Start: Initialize.", highlight=True)
        self.transition('q_init')

    def execute_q_init(self):
        self.registers['CurrentValue'] = self.registers['M']
        self.registers['Distance'] = 0
        self._record_history(f"Start at M ({self.registers['M']}). Target is S ({self.registers['S']}).")
        self.transition('q_check_status')

    def execute_q_check_status(self):
        if self.registers['CurrentValue'] > self.registers['S']:
            self.transition('q_init_K')
        else:
            self.Result = self.registers['Distance']
            self._record_history(f"Target reached. Result (Distance)={self.Result}.", highlight=True)
            self.transition('q_accept')

    def execute_q_init_K(self):
        """Initialize iterative calculation of K to reach the previous base."""
        self.registers['K'] = 0
        self.registers['internal_temp'] = self.registers['CurrentValue']
        
        current_val = self.registers['CurrentValue']
        target_base = current_val
        power = 1
        while True:
            base_power = self.Base**power
            if current_val % base_power != 0:
                target_base = (current_val // base_power) * base_power
                break
            if base_power > self.registers['M']:
                break
            power += 1
        self.registers['TargetBase'] = target_base

        self._record_history(f"Calculating K: Counting back from {current_val} to {target_base}.")
        self.transition('q_loop_K')

    def execute_q_loop_K(self):
        if self.registers['internal_temp'] > self.registers['TargetBase']:
            self.registers['internal_temp'] -= 1
            self.registers['K'] += 1
        else:
            self.transition('q_sub_chunk')

    def execute_q_sub_chunk(self):
        """Determine the chunk to subtract based on K or remaining distance."""
        remaining = self.registers['CurrentValue'] - self.registers['S']
        k = self.registers['K']
        
        if k > 0 and k <= remaining:
            chunk = k
            interpretation = f"Subtract strategic chunk (-{chunk}) to reach base."
        else:
            if remaining > 0:
                power = math.floor(math.log(remaining, self.Base))
                power_value = self.Base**power
                chunk = (remaining // power_value) * power_value
                chunk = chunk if chunk > 0 else remaining
                interpretation = f"Subtract large/remaining chunk (-{chunk})."
            else:
                self.transition('q_error'); return

        self.registers['Chunk'] = chunk
        self.registers['CurrentValue'] -= chunk
        self.registers['Distance'] += chunk
        self._record_history(interpretation + f" New Value={self.registers['CurrentValue']}.", highlight=True)
        self.transition('q_check_status')

if __name__ == '__main__':
    # Test Case 1: 400 - 294
    M_test = 400
    S_test = 294
    print(f"=== Test Case: {M_test} - {S_test} ===")
    auto_C = ChunkingAutomatonC(inputs={'M': M_test, 'S': S_test})
    result = auto_C.run()
    print(f"Final Result: {result}")
    auto_C.display_history()
    print("\n--- JSON Export ---")
    print(auto_C.export_trace_json())

    # Test Case 2: 83 - 17
    M_test_2 = 83
    S_test_2 = 17
    print(f"\n=== Test Case: {M_test_2} - {S_test_2} ===")
    auto_C_2 = ChunkingAutomatonC(inputs={'M': M_test_2, 'S': S_test_2})
    result_2 = auto_C_2.run()
    print(f"Final Result: {result_2}")
    auto_C_2.display_history()
