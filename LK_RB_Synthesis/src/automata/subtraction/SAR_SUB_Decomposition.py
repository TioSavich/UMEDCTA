import sys
import os
import json
import pandas as pd

# Add project root to path to allow importing from src
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../..')))

from src.automata.BaseAutomaton import BaseAutomaton
from src.analysis.MUA_Metadata import StrategyMetadata, EmbodiedMetaphor, MaterialInference
from typing import Dict

class DecompositionAutomaton(BaseAutomaton):
    """
    A Register Machine model simulating the 'Decomposition' (Borrowing) strategy for subtraction.
    Models the Left-to-Right approach: Subtract bases first, then ones, decomposing if necessary.
    """
    @property
    def metadata(self) -> StrategyMetadata:
        return StrategyMetadata(
            strategy_name="Decomposition (Borrowing)",
            description=(
                "Simulates the traditional 'borrowing' or 'decomposition' algorithm for subtraction. "
                "It processes numbers from left to right (or largest place value to smallest). "
                "If a digit in the minuend is smaller than the corresponding digit in the subtrahend, "
                "it 'borrows' from the next higher place value."
            ),
            metaphors=[
                EmbodiedMetaphor(
                    name="Arithmetic as Object Manipulation",
                    source_domain="Object Manipulation",
                    target_domain="Arithmetic",
                    entailments="A larger object (like a ten-block) can be broken down or exchanged for an equivalent set of smaller objects (ten one-blocks)."
                )
            ],
            inferences=[
                MaterialInference(
                    name="Place Value Equivalence",
                    premise="One unit of a higher place value (e.g., 1 Ten) is equivalent to a set number of units of the next lower place value (e.g., 10 Ones).",
                    conclusion="A unit from a higher place value can be removed and its equivalent value added to a lower place value without changing the total quantity.",
                    prerequisites=["Understanding of base-10 system", "Part-whole knowledge"]
                )
            ],
            visualization_hints=["Blocks"]
        )

    def __init__(self, inputs: Dict, Base=10):
        super().__init__(inputs, Base)
        self.registers.update({
            'M': self.inputs.get('M', 0),
            'S': self.inputs.get('S', 0),
            'S_T': 0, 'S_O': 0,
            'R_T': 0, 'R_O': 0
        })
        if self.registers['S'] > self.registers['M']:
            self.state = 'q_error'
            self._record_history(f"Error: Subtrahend ({self.registers['S']}) > Minuend ({self.registers['M']}).")

    def execute_q_start(self):
        self._record_history(f"Inputs: M={self.registers['M']}, S={self.registers['S']}", highlight=True)
        self.transition('q_init')

    def execute_q_init(self):
        """Decompose M and S into Tens and Ones."""
        self.registers['S_T'] = self.registers['S'] // self.Base
        self.registers['S_O'] = self.registers['S'] % self.Base
        self.registers['R_T'] = self.registers['M'] // self.Base
        self.registers['R_O'] = self.registers['M'] % self.Base
        
        self._record_history(f"Decompose M ({self.registers['R_T']}T+{self.registers['R_O']}O) and S ({self.registers['S_T']}T+{self.registers['S_O']}O).")
        self.transition('q_sub_bases')

    def execute_q_sub_bases(self):
        """Subtract the bases (Tens)."""
        initial_R_T = self.registers['R_T']
        self.registers['R_T'] -= self.registers['S_T']
        self._record_history(f"Subtract Bases: {initial_R_T}T - {self.registers['S_T']}T = {self.registers['R_T']}T.", highlight=True)
        self.transition('q_check_ones')

    def execute_q_check_ones(self):
        """Check if there are enough ones to subtract."""
        if self.registers['R_O'] >= self.registers['S_O']:
            self._record_history(f"Sufficient Ones ({self.registers['R_O']} >= {self.registers['S_O']}). Proceed.")
            self.transition('q_sub_ones')
        else:
            self._record_history(f"Insufficient Ones ({self.registers['R_O']} < {self.registers['S_O']}). Need decomposition.", highlight=True)
            self.transition('q_decompose')

    def execute_q_decompose(self):
        """Decompose (borrow) one ten into ones."""
        if self.registers['R_T'] > 0:
            self.registers['R_T'] -= 1
            self.registers['R_O'] += self.Base
            self._record_history(f"Decomposed 1 Ten. New state: {self.registers['R_T']}T, {self.registers['R_O']}O.", highlight=True)
            self.transition('q_sub_ones')
        else:
            self.transition('q_error')

    def execute_q_sub_ones(self):
        """Subtract the ones."""
        prev_O = self.registers['R_O']
        self.registers['R_O'] -= self.registers['S_O']
        self._record_history(f"Subtract Ones: {prev_O}O - {self.registers['S_O']}O = {self.registers['R_O']}O.", highlight=True)
        self.transition('q_accept')

    def execute_q_accept(self):
        """Combine results."""
        self.Result = self.registers['R_T'] * self.Base + self.registers['R_O']
        self._record_history(f"Accept. Final Result: {self.Result}.", highlight=True)
        super().execute_q_accept()

if __name__ == '__main__':
    # Test Case 1: Joel's example (45 - 27) - Requires Decomposition
    print("=== Test Case 1: 45 - 27 (Requires Decomposition) ===")
    decomp_45_27 = DecompositionAutomaton(inputs={'M': 45, 'S': 27})
    result1 = decomp_45_27.run()
    print(f"Final Result: {result1}")
    decomp_45_27.display_history()
    print("\n--- JSON Export ---")
    print(decomp_45_27.export_trace_json())

    # Test Case 2: No decomposition needed (48 - 23)
    print("\n=== Test Case 2: 48 - 23 (No Decomposition Needed) ===")
    decomp_48_23 = DecompositionAutomaton(inputs={'M': 48, 'S': 23})
    result2 = decomp_48_23.run()
    print(f"Final Result: {result2}")
    decomp_48_23.display_history()
