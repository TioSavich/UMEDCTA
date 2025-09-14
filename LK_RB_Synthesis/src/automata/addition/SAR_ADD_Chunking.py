import sys
import os
import json
import pandas as pd

# Add project root to path to allow importing from src
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../..')))

from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata, EmbodiedMetaphor, MaterialInference
from typing import Dict

class ChunkingAutomaton(BaseAutomaton):
    """
    A Register Machine model simulating the 'Chunking by Bases and Ones' strategy for addition.
    This strategy first adds the base-ten parts of the second number, then strategically
    adds the remaining ones, often by making a ten.
    """
    @property
    def metadata(self) -> StrategyMetadata:
        return StrategyMetadata(
            strategy_name="Chunking by Bases and Ones",
            description=(
                "Simulates an addition strategy where the second number (B) is decomposed into its base-ten and ones components. "
                "The base-ten part is added first, followed by a strategic addition of the ones, often involving 'making a ten' (RMB logic)."
            ),
            metaphors=[
                EmbodiedMetaphor(
                    name="Arithmetic as Object Manipulation",
                    source_domain="Object Manipulation",
                    target_domain="Arithmetic",
                    entailments="A collection can be augmented by adding other collections to it. It's often easier to add organized groups (like ten-blocks) first, then smaller items."
                )
            ],
            inferences=[
                MaterialInference(
                    name="Place Value Decomposition",
                    premise="A number can be decomposed into its constituent place value parts (e.g., 37 = 30 + 7).",
                    conclusion="Adding a number is equivalent to adding its parts sequentially.",
                    prerequisites=["Understanding of base-10 system"]
                ),
                MaterialInference(
                    name="Rounding to Make Bases (RMB)",
                    premise="It is easier to add from a number that is a multiple of the base.",
                    conclusion="A small quantity (K) can be added to reach a base, simplifying subsequent additions.",
                    prerequisites=["Part-whole knowledge"]
                )
            ],
            visualization_hints=["Blocks", "NumberLine"]
        )

    def __init__(self, inputs: Dict, Base=10):
        super().__init__(inputs, Base)
        self.registers.update({
            'A': self.inputs.get('A', 0),
            'B': self.inputs.get('B', 0),
            'Sum': 0,
            'BasesRemaining': 0,
            'OnesRemaining': 0,
            'K': 0,
            'internal_sum_temp': 0,
            'TargetBase': 0
        })

    def transition(self, next_state):
        # Reset K and internal counters when moving between major phases
        if next_state in ['q_init_ones_chunk', 'q_accept']:
             self.registers['K'] = 0
             self.registers['internal_sum_temp'] = 0
        super().transition(next_state)

    def execute_q_start(self):
        self._record_history(f"Inputs: A={self.registers['A']}, B={self.registers['B']}", highlight=True)
        self.transition('q_init')

    def execute_q_init(self):
        """Initialize Sum and decompose B."""
        self.registers['Sum'] = self.registers['A']
        self.registers['BasesRemaining'] = (self.registers['B'] // self.Base) * self.Base
        self.registers['OnesRemaining'] = self.registers['B'] % self.Base
        self._record_history(f"Initialize Sum to {self.registers['A']}. Decompose B: {self.registers['BasesRemaining']} + {self.registers['OnesRemaining']}.")
        self.transition('q_add_base_chunk')

    def execute_q_add_base_chunk(self):
        """Add the entire base chunk."""
        if self.registers['BasesRemaining'] > 0:
            chunk = self.registers['BasesRemaining']
            self.registers['Sum'] += chunk
            self.registers['BasesRemaining'] = 0
            self._record_history(f"Add Base Chunk (+{chunk}). Sum = {self.registers['Sum']}.", highlight=True)
        else:
            self._record_history("No bases to add.")
        self.transition('q_init_ones_chunk')

    def execute_q_init_ones_chunk(self):
        """Check if ones remain and transition accordingly."""
        if self.registers['OnesRemaining'] > 0:
            self._record_history(f"Begin strategic chunking of remaining ones ({self.registers['OnesRemaining']}).")
            self.transition('q_init_K')
        else:
            self._record_history("All ones added. Accepting.", highlight=True)
            self.transition('q_accept')

    def execute_q_init_K(self):
        """Initialize the 'Count Up To Base' subroutine."""
        self.registers['K'] = 0
        self.registers['internal_sum_temp'] = self.registers['Sum']
        
        current_sum = self.registers['Sum']
        if current_sum > 0 and current_sum % self.Base != 0:
             self.registers['TargetBase'] = ((current_sum // self.Base) + 1) * self.Base
        else:
             self.registers['TargetBase'] = current_sum
        
        self._record_history(f"Calculating K: Counting from {current_sum} to {self.registers['TargetBase']}.")
        self.transition('q_loop_K')

    def execute_q_loop_K(self):
        """Iteratively count up to the base."""
        if self.registers['internal_sum_temp'] < self.registers['TargetBase']:
            self.registers['internal_sum_temp'] += 1
            self.registers['K'] += 1
            self._record_history(f"Counting Up: {self.registers['internal_sum_temp']}, K={self.registers['K']}")
        else:
            self._record_history(f"K needed to reach base is {self.registers['K']}.")
            self.transition('q_add_ones_chunk')

    def execute_q_add_ones_chunk(self):
        """Apply the strategic chunk K or the remainder."""
        ones_rem = self.registers['OnesRemaining']
        k = self.registers['K']

        if ones_rem >= k and k > 0:
            chunk = k
            self.registers['Sum'] += chunk
            self.registers['OnesRemaining'] -= chunk
            self._record_history(f"Add Strategic Chunk (+{chunk}) to make base. Sum = {self.registers['Sum']}.", highlight=True)
        elif ones_rem > 0:
            chunk = ones_rem
            self.registers['Sum'] += chunk
            self.registers['OnesRemaining'] = 0
            self._record_history(f"Add Remaining Chunk (+{chunk}). Sum = {self.registers['Sum']}.", highlight=True)
        
        self.transition('q_init_ones_chunk')

    def execute_q_accept(self):
        self.Result = self.registers['Sum']
        super().execute_q_accept()

if __name__ == '__main__':
    # Test Case: Dionne's example (46 + 37)
    A_test = 46
    B_test = 37
    chunking_auto = ChunkingAutomaton(inputs={'A': A_test, 'B': B_test})
    result = chunking_auto.run()
    
    print(f"\n--- {chunking_auto.metadata.strategy_name} History ({A_test} + {B_test}) ---")
    print(f"Final Result: {result}")
    chunking_auto.display_history()
    
    print("\n--- Full Trace JSON Export ---")
    print(chunking_auto.export_trace_json())
